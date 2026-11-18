;;; eldoom-render.el --- Doom level renderer -*- lexical-binding:t -*-

;; Copyright (C) 2026 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;;; Code:

(require 'cl-lib)

(require 'eldoom-canvas)
(require 'eldoom-config)
(require 'eldoom-globals)
(require 'eldoom-info)
(require 'eldoom-intrusive)
(require 'eldoom-map)
(require 'eldoom-math)
(require 'eldoom-mobj)
(require 'eldoom-wad)

(defconst ern--sky-lightmap-index 0)

(defsubst ern--bam-signed (delta)
  "Map an unsigned BAM angle DELTA to a signed value in [-2^31, 2^31).
This is the BAM analogue of normalising an angle into (-pi, pi]."
  (declare (ftype (function (fixnum) fixnum)))
  (if (>= delta eldoom-math-ang180) (- delta eldoom-math-ang-max) delta))

(defsubst ern--view-to-point-angle (x-fx y-fx px-fx py-fx)
  "BAM angle from view point (X,Y) towards map point (PX,PY)."
  (declare (ftype (function (fixnum fixnum fixnum fixnum) fixnum)))
  (eldoom-math-atan2 (- py-fx y-fx) (- px-fx x-fx)))

(cl-defstruct (ern--scene
               (:constructor ern--make-scene)
               (:copier nil))
  view-transformation
  projection
  (center-x 0 :type fixnum)
  (center-y 0 :type fixnum)
  (half-fov 0 :type fixnum)
  projection-plane
  xtoviewangle   ;; BAM angle of each screen column relative to the view direction
  colormaps
  clip-top
  clip-bottom
  clip-solid
  visplane
  masked-drawseg
  vissprite
  drawseg
  draw-item
  sky-texture
  fixed-colormap
  extra-light)

(cl-defstruct (ern--visplane
               (:constructor ern--make-visplane)
               (:copier nil))
  (height 0 :type fixnum)        ;; World height of the plane
  texture                        ;; Swizzled flat lump, `sky', or legacy name string
  (light 0 :type fixnum)         ;; Sector light level
  (ceiling-p nil :type boolean)  ;; t if ceiling, nil if floor
  (min-x 0 :type fixnum)         ;; Starting screen column
  (max-x 0 :type fixnum)         ;; Ending screen column
  (top-vector (make-vector eldoom-canvas-width 0) :type array) ;; Array of top Y-bounds per column
  (bottom-vector (make-vector eldoom-canvas-width 0) :type array) ;; Array of bottom Y-bounds per column
  (extra-light 0 :type fixnum)   ;; Light adjustment from e.g. muzzle flash
  next)                          ;; Next visplane in a linked list

(define-eldoom-intrusive-freelist eldoom-render--visplane
                                  eldoom-render--visplane-next
                                  eldoom-render--make-visplane)

(cl-defstruct (ern--vissprite
               (:constructor ern--make-vissprite)
               (:copier nil))
  (x1 0 :type fixnum)            ;; Starting screen column
  (x2 0 :type fixnum)            ;; Ending screen column
  (scale-fx 0 :type fixnum)      ;; Fixed-point depth scale for rendering
  (texture-x-fx 0 :type fixnum)  ;; Starting texture X offset (fixed point)
  patch                          ;; The actual patch struct to draw
  colormap                       ;; The light table to use based on the sector the thing is in
  (mobj-z 0 :type fixnum)        ;; The Z height of the thing
  (texture-y-fx 0 :type fixnum)  ;; Starting vertical offset
  next)

(define-eldoom-intrusive-freelist eldoom-render--vissprite
                                  eldoom-render--vissprite-next
                                  eldoom-render--make-vissprite)

(defconst ern--rotation-angle-offset (* (/ eldoom-math-ang45 2) 9))
(defsubst ern--calculate-sprite-rotation (mobj view-x-fx view-y-fx)
  "Returns 0 (facing player) through 7
Part of R_ProjectSprite in r_things.c"
  (let* ((angle-to-player (eldoom-math-atan2 (- (emo-y-fx mobj) view-y-fx) (- (emo-x-fx mobj) view-x-fx)))
         (angle (+ (- angle-to-player (emo-angle mobj)) ern--rotation-angle-offset)))
    (ash (logand angle eldoom-math-ang-mask) -29)))

(defconst ern--min-sprite-view-distance (eldoom-math-fixed-from-int 8)
  "Things closer than this  are culled as behind the viewer.")

(cl-defstruct (ern--drawseg (:constructor ern--make-drawseg)
                            (:copier nil))
  (x1 0 :type fixnum)         ;; Leftmost screen column of the wall
  (x2 0 :type fixnum)         ;; Rightmost screen column of the wall
  (scale1-fx 0 :type fixnum)  ;; Fixed-point depth scale at x1
  (scale2-fx 0 :type fixnum)  ;; Fixed-point depth scale at x2
  type                        ;; Symbol: 'solid or 'portal
  (top-clip-array (make-vector eldoom-canvas-width 0) :type array) ;; Only used if 'portal
  (bottom-clip-array (make-vector eldoom-canvas-width 0) :type array) ;; Only used if 'portal
  next)

(define-eldoom-intrusive-freelist eldoom-render--drawseg
                                  eldoom-render--drawseg-next
                                  eldoom-render--make-drawseg)

(defun ern--store-drawseg (scene x1 x2 scale1-fx scale2-fx type)
  "Records a wall segment's screen bounds and depth for sprite clipping later."
  (let ((ds (ern--drawseg-new-from-freelist))
        (global-clip-top (ern--scene-clip-top scene))
        (global-clip-bottom (ern--scene-clip-bottom scene)))
    (setf (ern--drawseg-x1 ds) x1
          (ern--drawseg-x2 ds) x2
          (ern--drawseg-scale1-fx ds) scale1-fx
          (ern--drawseg-scale2-fx ds) scale2-fx
          (ern--drawseg-type ds) type
          (ern--drawseg-next ds) (ern--scene-drawseg scene)
          (ern--scene-drawseg scene) ds)

    ;; If it is a portal (window/step), we must save the silhouette limits
    ;; exactly as they were BEFORE this wall mutated them.
    (when (eq type 'portal)
      (let ((ds-top (ern--drawseg-top-clip-array ds))
            (ds-bot (ern--drawseg-bottom-clip-array ds)))
        (cl-loop for c from x1 to x2 do
                 (aset ds-top c (aref global-clip-top c))
                 (aset ds-bot c (aref global-clip-bottom c)))))))

(cl-defstruct (ern--masked-drawseg
               (:constructor ern--make-masked-drawseg)
               (:copier nil))
  scene
  column-start
  column-end
  texture
  (sector-light 0 :type fixnum)
  (light-adjust 0 :type fixnum)
  near-floor
  far-floor
  near-ceiling
  far-ceiling
  upper-unpegged-p
  lower-unpegged-p
  scale-start
  scale-step
  rw-distance
  rw-offset
  rw-centerangle
  texture-y-offset
  (clip-top-snapshot (make-vector eldoom-canvas-width 0) :type array)
  (clip-bottom-snapshot (make-vector eldoom-canvas-width 0) :type array)
  next)

(define-eldoom-intrusive-freelist eldoom-render--masked-drawseg
                                  eldoom-render--masked-drawseg-next
                                  eldoom-render--make-masked-drawseg)

(defconst ern--normal-colormap-count 32
  "Number of normal Doom light maps before special-purpose maps.")

(defvar-local ern--solid-column-count 0)

(defvar-local ern--sprite-clip-top (make-vector eldoom-canvas-width 0))
(defvar-local ern--sprite-clip-bottom (make-vector eldoom-canvas-width 0))

(defun ern--clip-sprite-against-drawsegs (sprite drawseg canvas-height)
  "Calculate the exact vertical clip bounds for a sprite based on all drawn walls."
  (fillarray ern--sprite-clip-top 0)
  (fillarray ern--sprite-clip-bottom (1- canvas-height))

  (let ((spr-x1 (ern--vissprite-x1 sprite))
        (spr-x2 (ern--vissprite-x2 sprite))
        (spr-scale (ern--vissprite-scale-fx sprite)))

    (while drawseg
      (let* ((ds-x1 (ern--drawseg-x1 drawseg))
             (ds-x2 (ern--drawseg-x2 drawseg)))

        ;; Check for horizontal overlap on the screen
        (when (and (<= ds-x1 spr-x2) (>= ds-x2 spr-x1))
          (let* ((clip-start (max spr-x1 ds-x1))
                 (clip-end (min spr-x2 ds-x2))
                 (ds-scale1 (ern--drawseg-scale1-fx drawseg))
                 (ds-scale2 (ern--drawseg-scale2-fx drawseg))
                 ;; Per-column scale change, kept as an exact integer ratio.
                 (ds-span (- ds-x2 ds-x1))
                 (ds-dscale (- ds-scale2 ds-scale1)))

            (if (eq (ern--drawseg-type drawseg) 'solid)
                ;; Solid Wall Clipping
                (cl-loop for c from clip-start to clip-end do
                         ;; Interpolate wall scale specifically for this column
                         (let ((wall-scale (if (= ds-span 0)
                                               ds-scale1
                                             (+ ds-scale1 (/ (* ds-dscale (- c ds-x1)) ds-span)))))
                           (when (> wall-scale spr-scale)
                             (aset ern--sprite-clip-top c 1000)
                             (aset ern--sprite-clip-bottom c -1000))))

              ;; Portal Window Clipping
              (let ((ds-top-array (ern--drawseg-top-clip-array drawseg))
                    (ds-bot-array (ern--drawseg-bottom-clip-array drawseg)))
                (cl-loop for c from clip-start to clip-end do
                         (let ((wall-scale (if (= ds-span 0)
                                               ds-scale1
                                             (+ ds-scale1 (/ (* ds-dscale (- c ds-x1)) ds-span)))))
                           (when (> wall-scale spr-scale)
                             (aset ern--sprite-clip-top c
                                   (max (aref ern--sprite-clip-top c)
                                        (aref ds-top-array c)))
                             (aset ern--sprite-clip-bottom c
                                   (min (aref ern--sprite-clip-bottom c)
                                        (aref ds-bot-array c)))))))))))
      (setq drawseg (ern--drawseg-next drawseg)))))

(defsubst ern--wall-light-adjust (seg)
  (let ((segment (ewd-seg-segment seg)))
    (cond
     ((= (eldoom-math-segment-y1 segment) (eldoom-math-segment-y2 segment)) -1)
     ((= (eldoom-math-segment-x1 segment) (eldoom-math-segment-x2 segment)) 1)
     (t 0))))

(defun ern--find-active-visplane (scene x height texture light ceiling-p)
  "Find a matching visplane that is not already active in column X."
  (let ((visplane (ern--scene-visplane scene))
        (found-visplane nil))
    (while (and visplane (not found-visplane))
      (if (and (= height (ern--visplane-height visplane))
               (= light (ern--visplane-light visplane))
               (eq ceiling-p (ern--visplane-ceiling-p visplane))
               (eq texture (ern--visplane-texture visplane))
               ;; Ensure we don't merge across a gap in the same column
               (= 10000 (aref (ern--visplane-top-vector visplane) x)))
          (setq found-visplane visplane)
        (setq visplane (ern--visplane-next visplane))))
    found-visplane))

(defun ern--valid-visplane-texture-p (tex)
  (or (eq tex 'sky)
      (let ((tex-data (ewd-flat-data tex)))
        (and (stringp tex-data) (= (length tex-data) 4096)))))

(defun ern--check-visplane
    (scene x top bottom height texture light ceiling-p)
  (when (and (<= top bottom)
             texture
             (ern--valid-visplane-texture-p texture))
    (let* ((vp (ern--find-active-visplane
                scene x height texture light ceiling-p)))
      (when (not vp)
        (setf vp (ern--visplane-new-from-freelist)
              (ern--visplane-next vp) (ern--scene-visplane scene)
              (ern--scene-visplane scene) vp)
        (fillarray (ern--visplane-top-vector vp) 10000)
        (fillarray (ern--visplane-bottom-vector vp) -1)
        (setf (ern--visplane-height vp) height
              (ern--visplane-texture vp) texture
              (ern--visplane-light vp) light
              (ern--visplane-ceiling-p vp) ceiling-p
              (ern--visplane-min-x vp) x
              (ern--visplane-max-x vp) x
              (ern--visplane-extra-light vp) (ern--scene-extra-light scene)))
      (setf (ern--visplane-min-x vp) (min (ern--visplane-min-x vp) x)
            (ern--visplane-max-x vp) (max (ern--visplane-max-x vp) x))
      (let ((top-vec (ern--visplane-top-vector vp))
            (bot-vec (ern--visplane-bottom-vector vp)))
        (aset top-vec x (min top (aref top-vec x)))
        (aset bot-vec x (max bottom (aref bot-vec x)))))))

;; Texture to use in place of a missing texture.
(defconst ern--fallback-color 251) ; pink
(defconst ern--fallback-texture (ewd-make-texture-from-color ern--fallback-color))

(defsubst ern--pack-span (start end)
  (logior (ash start 16) end))

(defsubst ern--draw-pixel (bitmap offset color-index colormap palette)
  (declare (ftype (function (array fixnum (or fixnum null) array array) t)))
  (when color-index
    (let* ((palette-index (aref colormap color-index))
           (color (aref palette palette-index)))
      (aset bitmap offset (aref color 0))
      (aset bitmap (1+ offset) (aref color 1))
      (aset bitmap (+ offset 2) (aref color 2)))))

(defun ern--draw-column-span
    (bitmap data-offset column draw-start draw-end
            texture-column tex-y-fx tex-y-step-fx
            colormap palette texture-height
            validate-range-p)
  "Draw one vertical column span (Doom R_DrawColumn / R_DrawMaskedColumn).
TEXTURE-COLUMN is a height-sized vector of palette indices (nil = transparent).
When VALIDATE-RANGE-P, sample only rows in [0, TEXTURE-HEIGHT) and skip nil.
Otherwise wrap with TEXTURE-HEIGHT."
  (declare (ftype (function ( array fixnum fixnum fixnum fixnum
                              array fixnum fixnum
                              array array fixnum
                              boolean) t)))
  ;; (declare (speed 3) (safety 0))
  (let ((offset (+ data-offset (* 3 (+ column (* draw-start eldoom-canvas-width)))))
        (stride (* 3 eldoom-canvas-width))
        (y draw-start))
    (if validate-range-p
        (while (<= y draw-end)
          (let ((tex-y-int (eldoom-math-fixed-to-int tex-y-fx)))
            (when (and (>= tex-y-int 0) (< tex-y-int texture-height))
              (ern--draw-pixel bitmap offset (aref texture-column tex-y-int) colormap palette)))
          (setq y (1+ y)
                tex-y-fx (+ tex-y-fx tex-y-step-fx)
                offset (+ offset stride)))
      (while (<= y draw-end)
        (ern--draw-pixel bitmap offset
                         (aref texture-column (mod (eldoom-math-fixed-to-int tex-y-fx) texture-height))
                         colormap palette)
        (setq y (1+ y)
              tex-y-fx (+ tex-y-fx tex-y-step-fx)
              offset (+ offset stride))))))

(defsubst ern--texture-world-top-z (wall-role upper-unpegged-p lower-unpegged-p
                                              near-ceiling near-floor far-ceiling far-floor
                                              texture-height)
  "World Z of texture row 0 for WALL-ROLE (Doom R_StoreWallRange pegging).
On two-sided lines ML_DONTPEGTOP/ML_DONTPEGBOTTOM control upper/lower textures;
the same bits select middle-texture pegging on masked midtextures.  On one-sided
lines only ML_DONTPEGBOTTOM applies, anchoring the middle texture to the floor."
  (pcase wall-role
    ('solid
     (if lower-unpegged-p
         (+ near-floor texture-height)
       near-ceiling))
    ('upper
     (if upper-unpegged-p
         near-ceiling
       (+ far-ceiling texture-height)))
    ('lower
     (if lower-unpegged-p
         near-ceiling
       far-floor))
    ('middle-portal
     (if lower-unpegged-p
         (+ (max near-floor far-floor) texture-height)
       (if upper-unpegged-p
           (max near-ceiling far-ceiling)
         (min near-ceiling far-ceiling))))))

(defun ern--draw-textured-span-clipped-fx
    (bitmap data-offset column clip-top clip-bottom
            wall-top-y-fx wall-bottom-y-fx world-top-z texture-height
            texture tex-x-fx texture-y-offset colormap palette
            view-z center-y-fx scale-fx &optional masked-p)
  (declare (ftype (function (array fixnum fixnum fixnum fixnum
                                   fixnum fixnum fixnum fixnum
                                   t fixnum fixnum t t
                                   fixnum fixnum fixnum &optional t) t)))
  ;; (declare (speed 3) (safety 0))
  (let* ((tex-top-y-fx (+ center-y-fx (* (- view-z world-top-z) scale-fx)))
         (tex-bottom-y-fx (+ center-y-fx
                             (* (- view-z (- world-top-z texture-height)) scale-fx)))
         (wall-start (eldoom-math-fixed-ceil wall-top-y-fx))
         (wall-end (eldoom-math-fixed-to-int wall-bottom-y-fx))
         (clip-start (max clip-top wall-start))
         (clip-end (min clip-bottom wall-end)))
    (when (<= clip-start clip-end)
      (let* ((denom-fx (max 1 (- tex-bottom-y-fx tex-top-y-fx)))
             (texture-width (cl-the fixnum (ewd-texture-width texture)))
             (texture-column (cl-the array (aref (ewd-texture-columns texture)
                                                 (mod (eldoom-math-fixed-to-int tex-x-fx) texture-width))))
             (step-fx (eldoom-math-div (eldoom-math-fixed-from-int texture-height) denom-fx))
             (tex-y-fx (+ (eldoom-math-fixed-from-int texture-y-offset)
                          (eldoom-math-mul (- (eldoom-math-fixed-from-int clip-start) tex-top-y-fx) step-fx))))
        (ern--draw-column-span
         bitmap data-offset column clip-start clip-end
         texture-column tex-y-fx step-fx colormap palette texture-height
         masked-p))
      (ern--pack-span clip-start clip-end))))

(defsubst ern--draw-wall-span-clipped-fx
    (bitmap data-offset column wall-top-y-fx wall-bottom-y-fx
            texture colormap palette tex-x-fx texture-y-offset
            clip-top clip-bottom wall-role
            upper-unpegged-p lower-unpegged-p near-ceiling near-floor
            far-ceiling far-floor view-z center-y-fx scale-fx)
  (declare (ftype (function (array fixnum fixnum fixnum fixnum
                                   t array array fixnum fixnum
                                   fixnum fixnum symbol
                                   t t fixnum fixnum
                                   fixnum fixnum fixnum fixnum fixnum) t)))
  (let* ((texture (or texture ern--fallback-texture))
         (texture-height (ewd-texture-height texture))
         (world-top-z (ern--texture-world-top-z wall-role upper-unpegged-p lower-unpegged-p
                                                near-ceiling near-floor far-ceiling far-floor
                                                texture-height)))
    (ern--draw-textured-span-clipped-fx
     bitmap data-offset column clip-top clip-bottom
     wall-top-y-fx wall-bottom-y-fx world-top-z texture-height
     texture tex-x-fx texture-y-offset colormap palette
     view-z center-y-fx scale-fx)))

(defsubst ern--span-start (span)
  (ash span -16))

(defsubst ern--span-end (span)
  (logand span #xffff))

(defun ern--draw-projected-column-clipped-fx
    (scene bitmap data-offset column
           near-floor near-floor-texture near-ceiling near-ceiling-texture
           far-floor far-ceiling far-sector
           middle-texture upper-texture lower-texture tex-x-fx
           texture-y-offset colormap palette light
           nc-y-fx nf-y-fx fc-y-fx ff-y-fx
           upper-unpegged-p lower-unpegged-p view-z scale-fx)
  "Draw a single vertical column of a wall segment."
  (declare (ftype (function (t array fixnum fixnum
                               fixnum array fixnum array
                               fixnum fixnum t
                               t t t fixnum
                               fixnum array array fixnum
                               fixnum fixnum fixnum fixnum
                               t t fixnum fixnum) t)))
  (let ((clip-solid-vector (ern--scene-clip-solid scene))
        (clip-top-vector (ern--scene-clip-top scene))
        (clip-bottom-vector (ern--scene-clip-bottom scene))
        (center-y-fx (ern--scene-center-y scene)))
    (unless (aref clip-solid-vector column)
      (let* ((clip-top (aref clip-top-vector column))
             (clip-bottom (aref clip-bottom-vector column))

             ;; --- THE DOOM SKY HACK ---
             (far-is-sky (and far-sector (eq (ewd-sector-ceiling-texture far-sector) 'sky))))

        (when (<= clip-top clip-bottom)
          (let* ((near-ceil-y (eldoom-math-fixed-ceil nc-y-fx))
                 (near-floor-y (eldoom-math-fixed-to-int nf-y-fx))

                 ;; Normal ceiling and floor visplane bounds for the near sector
                 (vis-ceil-bottom (min (1- near-ceil-y) clip-bottom (1- near-floor-y)))
                 (vis-floor-top (max near-floor-y clip-top near-ceil-y)))

            ;; 1. Check Near Ceiling Visplane
            (when (>= vis-ceil-bottom clip-top)
              (ern--check-visplane
               scene column clip-top vis-ceil-bottom
               near-ceiling near-ceiling-texture light t))

            ;; 2. The Doom Sky Upper-Wall Hack
            ;; If the far sector is sky, replace the upper wall gap with a sky visplane.
            (when (and far-is-sky (< far-ceiling near-ceiling))
              (let ((sky-gap-top (max clip-top near-ceil-y))
                    (sky-gap-bottom (min clip-bottom (eldoom-math-fixed-to-int fc-y-fx) (1- near-floor-y))))
                (when (<= sky-gap-top sky-gap-bottom)
                  (ern--check-visplane
                   scene column sky-gap-top sky-gap-bottom
                   far-ceiling 'sky ern--sky-lightmap-index t))))

            ;; 3. Check Near Floor Visplane
            (when (<= vis-floor-top clip-bottom)
              (ern--check-visplane
               scene column vis-floor-top clip-bottom
               near-floor near-floor-texture light nil)))

          (if (not far-sector)
              (progn
                ;; 1. SOLID WALL (1-sided line)
                (ern--draw-wall-span-clipped-fx
                 bitmap data-offset column
                 nc-y-fx nf-y-fx middle-texture colormap palette
                 tex-x-fx texture-y-offset
                 clip-top clip-bottom 'solid
                 upper-unpegged-p lower-unpegged-p
                 near-ceiling near-floor nil nil
                 view-z center-y-fx scale-fx)
                (aset clip-solid-vector column t))

            ;; 2. PORTAL (2-sided line)
            (let ((middle-drawn nil))

              ;; --- UPPER WALL & CEILING CLIP ---
              (if (< far-ceiling near-ceiling)
                  (progn
                    (unless far-is-sky
                      (ern--draw-wall-span-clipped-fx
                       bitmap data-offset column
                       nc-y-fx fc-y-fx upper-texture colormap palette
                       tex-x-fx texture-y-offset
                       clip-top clip-bottom 'upper
                       upper-unpegged-p lower-unpegged-p
                       near-ceiling near-floor far-ceiling far-floor
                       view-z center-y-fx scale-fx))
                    ;; Upper wall ends at floor(fc). Portal top must strictly start at floor(fc) + 1.
                    (aset clip-top-vector column (min (1+ clip-bottom)
                                                      (max clip-top (1+ (eldoom-math-fixed-to-int fc-y-fx)))))
                    (setq clip-top (aref clip-top-vector column)))

                ;; NO UPPER WALL: Portal top must perfectly abut the near ceiling flat.
                ;; Near ceiling flat ends at ceiling(nc) - 1. Portal must start at ceiling(nc).
                (aset clip-top-vector column (min (1+ clip-bottom) (max clip-top (eldoom-math-fixed-ceil nc-y-fx))))
                (setq clip-top (aref clip-top-vector column)))

              ;; --- LOWER WALL & FLOOR CLIP ---
              (if (> far-floor near-floor)
                  (progn
                    (ern--draw-wall-span-clipped-fx
                     bitmap data-offset column
                     ff-y-fx nf-y-fx lower-texture colormap palette
                     tex-x-fx texture-y-offset
                     clip-top clip-bottom 'lower
                     upper-unpegged-p lower-unpegged-p
                     near-ceiling near-floor far-ceiling far-floor
                     view-z center-y-fx scale-fx)
                    ;; Lower wall starts at ceiling(ff). Portal bottom must strictly end at ceiling(ff) - 1.
                    (aset clip-bottom-vector column (max (1- clip-top) (min clip-bottom (1- (eldoom-math-fixed-ceil ff-y-fx)))))
                    (setq clip-bottom (aref clip-bottom-vector column)))

                ;; NO LOWER WALL: Portal bottom must perfectly abut the near floor flat.
                ;; Near floor flat starts at floor(nf). Portal must end at floor(nf) - 1.
                (aset clip-bottom-vector column (max (1- clip-top) (min clip-bottom (1- (eldoom-math-fixed-to-int nf-y-fx)))))
                (setq clip-bottom (aref clip-bottom-vector column)))


              ;; --- MIDDLE WALL (Transparent Grate) ---
              (when (and (<= clip-top clip-bottom) middle-texture)
                (setq middle-drawn (ern--draw-wall-span-clipped-fx
                                    bitmap data-offset column
                                    nc-y-fx nf-y-fx middle-texture colormap palette
                                    tex-x-fx texture-y-offset
                                    clip-top clip-bottom 'middle-portal
                                    upper-unpegged-p lower-unpegged-p
                                    near-ceiling near-floor far-ceiling far-floor
                                    view-z center-y-fx scale-fx)))


              ;; Close the column if the portal collapsed mathematically, or a solid middle was drawn
              (when (or (> clip-top clip-bottom) middle-drawn)
                (aset clip-solid-vector column t)))))
        (when (aref clip-solid-vector column)
          (setf ern--solid-column-count (1+ ern--solid-column-count)))))
    nil))

(defun ern--draw-sky-visplane
    (vp bitmap data-offset sky-texture angle projection-plane center-x-fx center-y-fx colormap palette)
  "Draw a visplane using cylindrical sky projection."
  (declare (ftype (function (t array fixnum array number fixnum fixnum fixnum array array) t)))
  (let* ((min-x (ern--visplane-min-x vp))
         (max-x (ern--visplane-max-x vp))
         (top-vector (ern--visplane-top-vector vp))
         (bottom-vector (ern--visplane-bottom-vector vp))
         (center-y (eldoom-math-fixed-to-float center-y-fx)))
    (when sky-texture
      (let* ((tex-width (ewd-texture-width sky-texture))
             (tex-height (ewd-texture-height sky-texture))
             (tex-columns (ewd-texture-columns sky-texture))

             ;; The sky repeats 4 times over 360 degrees: a full circle (the BAM
             ;; range 2^32) maps to 1024 texture columns, i.e. a right shift of 22.

             ;; Scale vertically based strictly on Doom's native 200px logical height.
             (y-scale (/ 200.0 eldoom-canvas-height))

             ;; Vanilla Doom explicitly anchors the horizon to texture row 100.
             (horizon-anchor 100.0))

        (cl-loop for x from min-x to max-x do
                 (let ((top (aref top-vector x))
                       (bottom (aref bottom-vector x)))
                   (when (and (<= top bottom) (/= top 10000))
                     (let* ((col-offset (eldoom-math-atan2 (- (eldoom-math-fixed-from-int x) center-x-fx) projection-plane))
                            (ray-angle (eldoom-math-ang (- angle col-offset)))
                            (tex-x (mod (ash ray-angle -22) tex-width))
                            (texture-column (aref tex-columns tex-x))
                            (y top)
                            (offset (+ data-offset (* 3 (+ x (* y eldoom-canvas-width)))))
                            (stride (* 3 eldoom-canvas-width)))
                       (while (<= y bottom)

                         ;; Map screen Y to texture Y using the flat orthographic scale
                         (let* ((tex-y-float (+ (* (- y center-y) y-scale) horizon-anchor))
                                (tex-y (mod (floor tex-y-float) tex-height))
                                (color-index (aref texture-column tex-y)))
                           (ern--draw-pixel bitmap offset color-index colormap palette)
                           (setq y (1+ y)
                                 offset (+ offset stride))))))))))))


(defsubst ern--column-lightmap-index-fx (sector-light scale light-adjust)
  "Calculate the lightmap index for column with given vertical scale."
  (declare (ftype (function (fixnum fixnum fixnum) fixnum)))
  (let* ((adjusted-light (eldoom-math-clamp (+ (ash sector-light -3)
                                               (ash light-adjust 1))
                                            0 31))
         (scale-index (eldoom-math-clamp (ash scale -12) 0 47))
         (start-map (* (- 31 adjusted-light) 2))
         (light-index (- start-map scale-index)))
    (eldoom-math-clamp light-index 0 31)))

(defsubst ern--visplane-lightmap-index-fx (sector-light distance-fx light-adjust)
  "Calculate the lightmap for visplane row with given depth."
  (declare (ftype (function (fixnum fixnum fixnum) fixnum)))
  (let* ((adjusted-light (eldoom-math-clamp (+ (ash sector-light -3)
                                               (ash light-adjust 1))
                                            0 31))
         (z-index (eldoom-math-clamp (ash distance-fx -20) 0 127))
         (start-map (* (- 31 adjusted-light) 2))
         (z-bonus (/ 160 (1+ z-index) 2)))
    (eldoom-math-clamp (- start-map z-bonus) 0 31)))

(defun ern--find-visplane-colormap (scene sector-light distance-fx light-adjust)
  (let* ((fixed-colormap (ern--scene-fixed-colormap scene)))
    (if fixed-colormap
        fixed-colormap
      (let* ((colormaps (ern--scene-colormaps scene))
             (lightmap-index (ern--visplane-lightmap-index-fx sector-light distance-fx light-adjust)))
        (aref colormaps lightmap-index)))))

(defun ern--draw-visplanes
    (scene bitmap data-offset palette
           view-x-fx view-y-fx angle cos-fx sin-fx view-z)
  "Render all collected visplanes; flats horizontally and skies cylindrically."
  (declare (ftype (function (t array fixnum t
                               fixnum fixnum number number number number) t)))
  ;; (declare (speed 3) (safety 0))
  (let ((projection-plane (ern--scene-projection-plane scene))
        (sky-texture (ern--scene-sky-texture scene))
        (center-x-fx (ern--scene-center-x scene))
        (center-y-fx (ern--scene-center-y scene))
        (colormaps (ern--scene-colormaps scene))
        (visplane (ern--scene-visplane scene)))
    (while visplane
      (let* ((vp visplane)
             (flat (ern--visplane-texture vp)))
        (setq visplane (ern--visplane-next visplane))
        ;; Intercept the sky marker
        (if (eq flat 'sky)
            (ern--draw-sky-visplane
             vp bitmap data-offset sky-texture
             angle projection-plane center-x-fx center-y-fx
             (aref colormaps 0) ;; TODO: put correct colormap
             palette)

          (when (ern--valid-visplane-texture-p flat)
            (when-let* ((animation (ewd-flat-animation flat))
                        (base-frame (ewd-flat-animation-frame flat))
                        (frame-count (length animation))
                        (frame (mod (+ base-frame (/ eldoom-globals-level-time 8)) frame-count)))
              (setq flat (aref animation frame)))

            (let* ((flat-data (ewd-flat-data flat))
                   (light (ern--visplane-light vp))
                   (extra-light (ern--visplane-extra-light vp))
                   (ceiling-p (ern--visplane-ceiling-p vp))
                   (plane-height (if ceiling-p
                                     (- (ern--visplane-height vp) view-z)
                                   (- view-z (ern--visplane-height vp))))
                   (min-x (ern--visplane-min-x vp))
                   (max-x (ern--visplane-max-x vp))
                   (top-vector (ern--visplane-top-vector vp))
                   (bottom-vector (ern--visplane-bottom-vector vp))
                   (plane-height-fx (eldoom-math-fixed-from-int plane-height))
                   (min-y eldoom-canvas-height)
                   (max-y 0))
              ;; 1. Find the absolute Y bounds
              (let ((x min-x))
                (while (<= x max-x)
                  (let ((top (aref top-vector x))
                        (bot (aref bottom-vector x)))
                    (when (and (<= top bot) (/= top 10000))
                      (setq min-y (min min-y top)
                            max-y (max max-y bot))))
                  (setq x (1+ x))))

              ;; 2. Y-Axis Projection Loop
              (let ((y min-y))
                (while (<= y max-y)
                  (let* ((sample-y-fx (+ (eldoom-math-fixed-from-int y) 32768))
                         (denom-fx (if ceiling-p
                                       (- center-y-fx sample-y-fx)
                                     (- sample-y-fx center-y-fx))))
                    (when (> denom-fx 4)      ; ~epsilon, in fixed-point pixels
                      ;; distance = planeheight * (centerx / denom); the division
                      ;; comes first (Doom yslope) to keep FixedDiv from saturating.
                      (let* ((yslope-fx (eldoom-math-div center-x-fx denom-fx))
                             (distance-fx (eldoom-math-mul plane-height-fx yslope-fx))
                             (colormap (ern--find-visplane-colormap scene light
                                                                    distance-fx extra-light))
                             (fx-step (eldoom-math-mul distance-fx (eldoom-math-div sin-fx projection-plane)))
                             (fy-step (eldoom-math-mul distance-fx (eldoom-math-div cos-fx projection-plane)))
                             (x min-x))

                        ;; 3. Span Extraction Loop
                        (while (<= x max-x)
                          ;; Fast-forward through empty space
                          (while (and (<= x max-x)
                                      (or (< y (aref top-vector x))
                                          (> y (aref bottom-vector x))))
                            (setq x (1+ x)))

                          (when (<= x max-x)
                            (let ((span-start x))
                              ;; Find the end of this contiguous span
                              (while (and (<= x max-x)
                                          (<= (aref top-vector x) y)
                                          (<= y (aref bottom-vector x)))
                                (setq x (1+ x)))
                              (let ((span-end (1- x)))

                                ;; Algebraic Step Jumping:
                                ;; Calculate exact texture coordinates directly for the span start
                                (let* ((side-fx (eldoom-math-div (- center-x-fx (+ (eldoom-math-fixed-from-int span-start) 32768))
                                                                 projection-plane))
                                       (world-x-fx (+ view-x-fx
                                                      (eldoom-math-mul distance-fx
                                                                       (- cos-fx (eldoom-math-mul side-fx sin-fx)))))
                                       (world-y-fx (+ view-y-fx
                                                      (eldoom-math-mul distance-fx
                                                                       (+ sin-fx (eldoom-math-mul side-fx cos-fx)))))
                                       (fx world-x-fx)
                                       (fy (- world-y-fx))
                                       (draw-x span-start)
                                       (offset (+ data-offset (* 3 (+ draw-x (* y eldoom-canvas-width))))))

                                  (while (<= draw-x span-end)
                                    (let* ((color-offset (+ (logand (eldoom-math-fixed-to-int fx) 63)
                                                            (* 64 (logand (eldoom-math-fixed-to-int fy) 63))))
                                           (color-index (aref flat-data color-offset)))
                                      (ern--draw-pixel bitmap offset color-index colormap palette))

                                    (setq draw-x (1+ draw-x)
                                          offset (+ offset 3)
                                          fx (+ fx fx-step)
                                          fy (+ fy fy-step)))))))))))
                  (setq y (1+ y)))))))))))

(defun ern--make-xtoviewangle (center-x-fx projection-plane-fx)
  "Build Doom's xtoviewangle table: the BAM angle of each screen column.
For column X, the angle is atan((centerx - x) / projection), so columns
left of centre have positive (counter-clockwise) angles, like vanilla."
  (let ((table (make-vector (1+ eldoom-canvas-width) 0)))
    (dotimes (x (1+ eldoom-canvas-width))
      (aset table x
            (eldoom-math-atan2 (- center-x-fx (eldoom-math-fixed-from-int x)) projection-plane-fx)))
    table))

(defun ern-scene-for-map (map)
  (let* ((half-fov (/ eldoom-fov 2.0))
         (projection-plane (/ (/ eldoom-canvas-width 2.0) (tan half-fov)))
         (center-x-fx (eldoom-math-fixed-from-float (/ eldoom-canvas-width 2.0)))
         (projection-plane-fx (eldoom-math-fixed-from-float projection-plane))
         (scene
          (ern--make-scene
           :view-transformation (eldoom-math-make-view-transformation)
           :projection (eldoom-math-make-projection)
           :center-x center-x-fx
           :center-y (eldoom-math-fixed-from-float (/ eldoom-canvas-height 2.0))
           :half-fov (eldoom-math-radians-to-bam (+ half-fov 0.15))
           :projection-plane projection-plane-fx
           :xtoviewangle (ern--make-xtoviewangle center-x-fx projection-plane-fx)
           :colormaps (ewd-map-colormaps map)
           :clip-top nil
           :clip-bottom nil
           :clip-solid nil
           :visplane nil
           :masked-drawseg nil
           :vissprite nil
           :drawseg nil
           :sky-texture (ewd-map-sky-texture map))))
    (eldoom-math-set-projection (ern--scene-projection scene)
                                (eldoom-math-fixed-from-int eldoom-canvas-width)
                                (eldoom-math-fixed-from-int eldoom-canvas-height)
                                (eldoom-math-radians-to-bam eldoom-fov))
    scene))

(defun ern--ensure-clip-vector (vector width initial-value)
  (if (and vector (= (length vector) width))
      vector
    (make-vector width initial-value)))

(defun ern--ensure-clip-solid-vector (vector width initial-value)
  (if (and vector (= (length vector) width))
      vector
    (make-bool-vector width initial-value)))

(defun ern--clear-clips (scene width height)
  (let ((clip-top (ern--ensure-clip-vector
                   (ern--scene-clip-top scene)
                   width 0))
        (clip-bottom (ern--ensure-clip-vector
                      (ern--scene-clip-bottom scene)
                      width (1- height)))
        (clip-solid (ern--ensure-clip-solid-vector
                     (ern--scene-clip-solid scene)
                     width nil)))
    (fillarray clip-top 0)
    (fillarray clip-bottom (1- height))
    (fillarray clip-solid nil)
    (setf (ern--scene-clip-top scene) clip-top)
    (setf (ern--scene-clip-bottom scene) clip-bottom)
    (setf (ern--scene-clip-solid scene) clip-solid)
    (setf ern--solid-column-count 0)))

(defun ern--clear-visplanes (scene)
  "Moves all visplanes from visplane list to freelist"
  (let ((vp (ern--scene-visplane scene)))
    (while vp
      (let ((vp-next (ern--visplane-next vp)))
        (ern--visplane-move-to-freelist vp)
        (setq vp vp-next))))
  (setf (ern--scene-visplane scene) nil))

(defun ern--clear-masked-drawsegs (scene)
  "Moves all masked drawsegs from drawseg list to freelist"
  (let ((masked-drawseg (ern--scene-masked-drawseg scene)))
    (while masked-drawseg
      (let ((masked-drawseg-next (ern--masked-drawseg-next masked-drawseg)))
        (ern--masked-drawseg-move-to-freelist masked-drawseg)
        (setq masked-drawseg masked-drawseg-next))))
  (setf (ern--scene-masked-drawseg scene) nil))

(defun ern--clear-vissprites (scene)
  "Moves all vissprites from vissprite list to freelist"
  (let ((vissprite (ern--scene-vissprite scene)))
    (while vissprite
      (let ((vissprite-next (ern--vissprite-next vissprite)))
        (ern--vissprite-move-to-freelist vissprite)
        (setq vissprite vissprite-next))))
  (setf (ern--scene-vissprite scene) nil))

(defun ern--clear-drawsegs (scene)
  "Moves all drawsegs from drawseg list to freelist"
  (let ((drawseg (ern--scene-drawseg scene)))
    (while drawseg
      (let ((drawseg-next (ern--drawseg-next drawseg)))
        (ern--drawseg-move-to-freelist drawseg)
        (setq drawseg drawseg-next))))
  (setf (ern--scene-drawseg scene) nil))

(defsubst ern--point-visible-p (px-fx py-fx x-fx y-fx angle half-fov)
  "Pure math check for point visibility, allocating no cons cells.
ANGLE and HALF-FOV are BAM angles."
  (<= (abs (ern--bam-signed (eldoom-math-ang (- (ern--view-to-point-angle x-fx y-fx px-fx py-fx) angle))))
      half-fov))

(defun ern--ray-intersect-aabb-p (x-fx y-fx dx-fx dy-fx top-fx bottom-fx left-fx right-fx)
  "Check if a 2D ray starting at (X, Y) with direction (DX, DY) intersects an AABB."
  (let ((tmin eldoom-math-int-min)
        (tmax eldoom-math-int-max))
    (if (= dx-fx 0)
        (unless (and (<= left-fx x-fx) (<= x-fx right-fx))
          (setq tmin eldoom-math-unit
                tmax (- eldoom-math-unit)))
      (let ((t1 (eldoom-math-div (- left-fx x-fx) dx-fx))
            (t2 (eldoom-math-div (- right-fx x-fx) dx-fx)))
        (when (> t1 t2) (cl-rotatef t1 t2))
        (setq tmin (max tmin t1)
              tmax (min tmax t2))))
    (if (= dy-fx 0)
        (unless (and (<= bottom-fx y-fx) (<= y-fx top-fx))
          (setq tmin eldoom-math-unit
                tmax (- eldoom-math-unit)))
      (let ((t1 (eldoom-math-div (- bottom-fx y-fx) dy-fx))
            (t2 (eldoom-math-div (- top-fx y-fx) dy-fx)))
        (when (> t1 t2) (cl-rotatef t1 t2))
        (setq tmin (max tmin t1)
              tmax (min tmax t2))))
    (and (<= tmin tmax) (>= tmax 0))))

(defun ern--bbox-visible-p (bbox x-fx y-fx angle half-fov)
  (let ((top-fx (eldoom-math-bbox-top bbox))
        (bottom-fx (eldoom-math-bbox-bottom bbox))
        (left-fx (eldoom-math-bbox-left bbox))
        (right-fx (eldoom-math-bbox-right bbox)))
    (if (and (<= left-fx x-fx) (<= x-fx right-fx) (<= bottom-fx y-fx) (<= y-fx top-fx))
        t ;; 1. Camera is physically inside the bounding box
      (if (or (ern--point-visible-p left-fx bottom-fx x-fx y-fx angle half-fov)
              (ern--point-visible-p left-fx top-fx x-fx y-fx angle half-fov)
              (ern--point-visible-p right-fx bottom-fx x-fx y-fx angle half-fov)
              (ern--point-visible-p right-fx top-fx x-fx y-fx angle half-fov))
          t ;; 2. Any corner is inside the view frustum

        ;; 3. The bounding box is so large it straddles the frustum rays.
        ;; We test if the left or right FOV bounding rays intersect the AABB.
        (let ((left-ray-dx (eldoom-math-cos (eldoom-math-ang (+ angle half-fov))))
              (left-ray-dy (eldoom-math-sin (eldoom-math-ang (+ angle half-fov))))
              (right-ray-dx (eldoom-math-cos (eldoom-math-ang (- angle half-fov))))
              (right-ray-dy (eldoom-math-sin (eldoom-math-ang (- angle half-fov)))))
          (or (ern--ray-intersect-aabb-p x-fx y-fx left-ray-dx left-ray-dy top-fx bottom-fx left-fx right-fx)
              (ern--ray-intersect-aabb-p x-fx y-fx right-ray-dx right-ray-dy top-fx bottom-fx left-fx right-fx)))))))

(defun ern--find-vissprite-colormap (mobj sector-light scale-fx fixed-colormap
                                          colormaps light-adjust)
  (cond
   ;; ((eldoom-math-has-flag (emo-flags mobj) enf-MF_SHADOW)
   ;; TODO: handle shadow draws
   (fixed-colormap fixed-colormap)
   ((enf-state-fullbright (emo-state mobj)) (aref colormaps 0))
   (t
    (let* ((lightmap-index (ern--column-lightmap-index-fx sector-light scale-fx light-adjust)))
      (aref colormaps lightmap-index)))))

(defun ern--try-queue-vissprite
    (scene mobj sector x-fx y-fx cos-fx sin-fx view-z)
  "R_ProjectSprite-like from r_things.c"
  (let* ((projection-plane (ern--scene-projection-plane scene))
         (colormaps (ern--scene-colormaps scene))
         (center-x-fx (ern--scene-center-x scene))
         (center-y-fx (ern--scene-center-y scene))
         (fixed-colormap (ern--scene-fixed-colormap scene))
         (extra-light (ern--scene-extra-light scene))
         (light (ewd-sector-light sector)))
    (let* ((tx-fx (emo-x-fx mobj))
           (ty-fx (emo-y-fx mobj))
           (tr-x-fx (- tx-fx x-fx))
           (tr-y-fx (- ty-fx y-fx))
           ;; Integer map deltas times fixed cos/sin yield fixed view-space.
           (front (+ (eldoom-math-mul tr-x-fx cos-fx) (eldoom-math-mul tr-y-fx sin-fx)))
           (side (+ (eldoom-math-mul tr-x-fx (- sin-fx)) (eldoom-math-mul tr-y-fx cos-fx))))
      ;; Only check if it's behind the camera. Drop the strict FOV angle check.
      (when (> front ern--min-sprite-view-distance)
        (let* ((rotation (ern--calculate-sprite-rotation mobj x-fx y-fx))
               (sprite (emo-sprite mobj))
               (patch (aref (aref sprite (emo-frame mobj)) rotation)))
          (let* ((patch-width (ewd-patch-width patch))
                 (patch-x-offset (ewd-patch-x-offset patch))
                 (patch-y-offset (ewd-patch-y-offset patch))
                 (scale-fx (eldoom-math-div projection-plane front))
                 (screen-x-fx (- center-x-fx (eldoom-math-mul projection-plane (eldoom-math-div side front))))

                 (real-x1 (eldoom-math-fixed-to-int (- screen-x-fx (* patch-x-offset scale-fx))))
                 (x1 (max 0 real-x1))
                 (x2 (min (1- eldoom-canvas-width)
                          (eldoom-math-fixed-to-int (- (+ screen-x-fx
                                                          (* (- patch-width patch-x-offset) scale-fx))
                                                       eldoom-math-unit)))))
            ;; Fix 2: Wait until AFTER projection to cull, allowing partial off-screen draws
            (when (and (<= x1 x2)
                       (<= x1 (1- eldoom-canvas-width))
                       (>= x2 0))
              (let* ((thing-z (eldoom-math-fixed-to-int (emo-z-fx mobj)))
                     (colormap (ern--find-vissprite-colormap mobj light scale-fx
                                                             fixed-colormap colormaps extra-light))
                     ;; top-y = center - (thing-z + y-offset - view-z) * scale
                     (top-y-fx (- center-y-fx
                                  (* (- (+ thing-z patch-y-offset) view-z) scale-fx)))
                     (vissprite (ern--vissprite-new-from-freelist)))
                (setf (ern--vissprite-next vissprite) (ern--scene-vissprite scene)
                      (ern--scene-vissprite scene) vissprite
                      (ern--vissprite-x1 vissprite) x1
                      (ern--vissprite-x2 vissprite) x2
                      (ern--vissprite-scale-fx vissprite) scale-fx
                      (ern--vissprite-texture-x-fx vissprite) real-x1
                      (ern--vissprite-patch vissprite) patch
                      (ern--vissprite-colormap vissprite) colormap
                      (ern--vissprite-mobj-z vissprite) thing-z
                      (ern--vissprite-texture-y-fx vissprite) top-y-fx)))))))))

(cl-defstruct (ern--texture-mapping
               (:constructor ern--make-texture-mapping)
               (:copier nil))
  (distance 0 :type fixnum)
  (offset 0 :type fixnum)
  (center-angle 0 :type fixnum))
(defvar-local ern--seg-texture-mapping nil)

(defun ern--seg-texture-mapping (front1 side1 front2 side2 seg sidedef)
  "Doom R_StoreWallRange texture setup from view-space seg endpoints.
FRONT1/SIDE1 and FRONT2/SIDE2 are v1/v2 after
`eldoom-math-apply-view-transformation'.
RW-DISTANCE is |cross|/LENGTH, RW-OFFSET is signed |dot|/LENGTH at v1 (vanilla
side rule), RW-CENTERANGLE is the negated view-space seg bearing."
  (when (not ern--seg-texture-mapping) (setf ern--seg-texture-mapping (ern--make-texture-mapping)))
  (let* ((df (- front2 front1))
         (ds (- side2 side1))
         (seg-angle (eldoom-math-atan2 ds df))
         (seg-len (max 1 (ewd-seg-length-fx seg)))
         (cross (- (eldoom-math-mul front1 ds) (eldoom-math-mul side1 df)))
         (dot (+ (eldoom-math-mul front1 df) (eldoom-math-mul side1 ds)))
         (rw-distance (eldoom-math-div (abs cross) seg-len))
         (along (eldoom-math-div dot seg-len))
         (rw-offset (+ (- (eldoom-math-fixed-from-int (+ (ewd-sidedef-x sidedef)
                                                         (ewd-seg-offset seg)))
                          along)
                       (ewd-sidedef-texture-offset sidedef)))
         (rw-centerangle (eldoom-math-ang (- seg-angle))))
    (setf (ern--texture-mapping-distance ern--seg-texture-mapping) rw-distance
          (ern--texture-mapping-offset ern--seg-texture-mapping) rw-offset
          (ern--texture-mapping-center-angle ern--seg-texture-mapping) rw-centerangle)
    ern--seg-texture-mapping))

(defconst ern--finetangent-half (/ eldoom-math-fine-angles 2)
  "Number of entries in `eldoom-math-finetangent' (covers -90..90 degrees).")

(defsubst ern--texturecolumn-fx (rw-offset-fx rw-distance-fx rw-centerangle xtoviewangle-x)
  "Fixed-point texture column for one screen column (Doom R_RenderSegLoop).
XTOVIEWANGLE-X is the precomputed BAM angle of that column."
  (let ((combined (+ rw-centerangle xtoviewangle-x)))
    (+ rw-offset-fx (eldoom-math-mul (eldoom-math-cot combined) rw-distance-fx))))

(defsubst ern--scale-from-global-angle (xtoviewangle-x rw-centerangle rw-distance-fx
                                                       projection-fx)
  "Doom R_ScaleFromGlobalAngle: the perspective scale of one screen column.
Computed straight from the constant perpendicular RW-DISTANCE-FX and the
column angle, so it stays well-conditioned even when a wall endpoint crosses
behind the viewer.  Deriving the scale instead from PROJECTION / near-clipped
depth and extrapolating it across the span suffers catastrophic cancellation
there, which both bends the wall and corrupts sprite-vs-wall occlusion."
  (let* ((angleb (eldoom-math-ang (+ rw-centerangle xtoviewangle-x)))
         (anglea (eldoom-math-ang (+ eldoom-math-ang90 xtoviewangle-x)))
         (num (eldoom-math-mul projection-fx (eldoom-math-sin angleb)))
         (den (eldoom-math-mul rw-distance-fx (eldoom-math-sin anglea))))
    (if (> den (ash num -16))
        (eldoom-math-clamp (eldoom-math-div num den) 256 (ash 64 16))
      (ash 64 16))))

(defun ern--animated-texture (texture)
  (when texture
    (if (ewd-texture-animation texture)
        (let* ((animation (ewd-texture-animation texture))
               (base-frame (ewd-texture-animation-frame texture))
               (frame-count (length animation))
               (frame (mod (+ base-frame (/ eldoom-globals-level-time 8)) frame-count)))
          (aref animation frame))
      texture)))

(defun ern--find-drawseg-colormap (scene sector-light scale-fx light-adjust)
  (declare (ftype (function (ern--scene fixnum fixnum) array)))
  (let* ((fixed-colormap (ern--scene-fixed-colormap scene)))
    (if fixed-colormap
        fixed-colormap
      (let* ((colormaps (ern--scene-colormaps scene))
             (colormap-index (ern--column-lightmap-index-fx sector-light scale-fx light-adjust)))
        (aref colormaps colormap-index)))))

(defun ern--draw-candidate-projected
    (scene bitmap data-offset
           seg near-sector far-sector sidedef
           view-z
           palette)
  (let* ((near-plane-fx (eldoom-math-fixed-from-int 4))            ; 4.0 world units
         (projection-plane (ern--scene-projection-plane scene))
         (view-transformation (ern--scene-view-transformation scene))
         (projection (ern--scene-projection scene))
         (xtoviewangle (ern--scene-xtoviewangle scene))
         (clip-top-vector (ern--scene-clip-top scene))
         (clip-bottom-vector (ern--scene-clip-bottom scene))
         (clip-solid-vector (ern--scene-clip-solid scene))
         (center-y-fx (ern--scene-center-y scene))
         (linedef-flags (ewd-linedef-flags (ewd-seg-linedef seg)))
         (upper-unpegged-p (/= 0 (logand linedef-flags 8)))
         (lower-unpegged-p (/= 0 (logand linedef-flags 16)))
         (middle-texture (ern--animated-texture (ewd-sidedef-middle sidedef)))
         (near-floor-height (eldoom-math-fixed-to-int (ewd-sector-floor-height-fx near-sector)))
         (near-floor-texture (ewd-sector-floor-texture near-sector))
         (near-ceiling-height (eldoom-math-fixed-to-int (ewd-sector-ceiling-height-fx near-sector)))
         (near-ceiling-texture (ewd-sector-ceiling-texture near-sector))
         (far-floor-height (when far-sector
                             (eldoom-math-fixed-to-int (ewd-sector-floor-height-fx far-sector))))
         (far-ceiling-height (when far-sector
                               (eldoom-math-fixed-to-int (ewd-sector-ceiling-height-fx far-sector))))
         (upper-texture (ern--animated-texture (ewd-sidedef-upper sidedef)))
         (lower-texture (ern--animated-texture (ewd-sidedef-lower sidedef)))
         (light-adjust (+ (ern--wall-light-adjust seg) (ern--scene-extra-light scene)))
         (texture-y-offset (ewd-sidedef-y sidedef))
         (segment (ewd-seg-segment seg))
         (x1-fx (eldoom-math-segment-x1 segment))
         (y1-fx (eldoom-math-segment-y1 segment))
         (x2-fx (eldoom-math-segment-x2 segment))
         (y2-fx (eldoom-math-segment-y2 segment))
         (front1 (eldoom-math-apply-view-transformation-x
                  view-transformation x1-fx y1-fx))
         (side1 (eldoom-math-apply-view-transformation-y
                 view-transformation x1-fx y1-fx))
         (front2 (eldoom-math-apply-view-transformation-x
                  view-transformation x2-fx y2-fx))
         (side2 (eldoom-math-apply-view-transformation-y
                 view-transformation x2-fx y2-fx))
         ;; Doom-style texture mapping: constant perpendicular distance and
         ;; offset for the whole seg, plus a per-column angle (R_RenderSegLoop).
         (rw-mapping (ern--seg-texture-mapping front1 side1 front2 side2 seg sidedef))
         (rw-distance-fx (ern--texture-mapping-distance rw-mapping))
         (rw-offset-fx (ern--texture-mapping-offset rw-mapping))
         (rw-centerangle (ern--texture-mapping-center-angle rw-mapping)))
    (unless (and (<= front1 near-plane-fx) (<= front2 near-plane-fx))
      ;; Near-plane clip of the view-space endpoints.  The texture mapping is
      ;; angle-based (rw-distance/rw-offset/rw-centerangle), so it needs no
      ;; clipping here: the per-column angle is correct for any visible column.
      (when (<= front1 near-plane-fx)
        (let ((amount (eldoom-math-div (- near-plane-fx front1) (- front2 front1))))
          (setq front1 near-plane-fx
                side1 (+ side1 (eldoom-math-mul amount (- side2 side1))))))
      (when (<= front2 near-plane-fx)
        (let ((amount (eldoom-math-div (- near-plane-fx front2) (- front1 front2))))
          (setq front2 near-plane-fx
                side2 (+ side2 (eldoom-math-mul amount (- side1 side2))))))
      (let* ((screen-x1-fx (eldoom-math-apply-projection-x projection side1 front1))
             (screen-x2-fx (eldoom-math-apply-projection-x projection side2 front2))
             (screen-dx-fx (- screen-x2-fx screen-x1-fx)))
        (unless (< (abs screen-dx-fx) 4)        ; sub-pixel-thin: skip degenerate wall

          ;; Guarantees the pixel center (+ 0.5) is ALWAYS inside the segment.
          ;; Mathematically prevents Z-extrapolation (ceiling pillars).
          (let* ((raw-start (eldoom-math-fixed-ceil (- screen-x1-fx 32768)))
                 (raw-end (eldoom-math-fixed-to-int (- screen-x2-fx 32768))))

            (when (and (<= raw-start (1- eldoom-canvas-width)) (>= raw-end 0) (<= raw-start raw-end))
              (let* ((start-column (eldoom-math-clamp raw-start 0 (1- eldoom-canvas-width)))
                     (end-column (eldoom-math-clamp raw-end 0 (1- eldoom-canvas-width)))
                     ;; scale = projection / depth, evaluated directly at the
                     ;; visible columns via R_ScaleFromGlobalAngle.  Linear in
                     ;; screen x, so interpolating between these two accurate
                     ;; endpoints reproduces every column (used for vertical
                     ;; positioning, lighting and sprite occlusion).
                     (scale1-fx (ern--scale-from-global-angle
                                 (aref xtoviewangle start-column)
                                 rw-centerangle rw-distance-fx projection-plane))
                     (scale2-fx (ern--scale-from-global-angle
                                 (aref xtoviewangle end-column)
                                 rw-centerangle rw-distance-fx projection-plane))
                     (scale-span (- end-column start-column))
                     (scale-step-fx (if (= scale-span 0) 0
                                      (/ (- scale2-fx scale1-fx) scale-span)))
                     (scale-start-fx scale1-fx)

                     ;; Heights relative to the viewer (integer world units).
                     (k-near-ceil (- view-z near-ceiling-height))
                     (k-near-floor (- view-z near-floor-height))
                     (k-far-ceil (when far-ceiling-height (- view-z far-ceiling-height)))
                     (k-far-floor (when far-floor-height (- view-z far-floor-height)))

                     ;; Screen Y = center + height * scale (integer * fixed = fixed).
                     (nc-y-fx (+ center-y-fx (* k-near-ceil scale-start-fx)))
                     (nf-y-fx (+ center-y-fx (* k-near-floor scale-start-fx)))
                     (fc-y-fx (when k-far-ceil (+ center-y-fx (* k-far-ceil scale-start-fx))))
                     (ff-y-fx (when k-far-floor (+ center-y-fx (* k-far-floor scale-start-fx))))

                     ;; Per-single-column Y steps (loop-invariant).
                     (nc-y-step-fx (* k-near-ceil scale-step-fx))
                     (nf-y-step-fx (* k-near-floor scale-step-fx))
                     (fc-y-step-fx (when k-far-ceil (* k-far-ceil scale-step-fx)))
                     (ff-y-step-fx (when k-far-floor (* k-far-floor scale-step-fx)))
                     (column start-column)
                     (wall-type (if far-sector 'portal 'solid))
                     ;; Drawseg scale bounds for later sprite clipping.
                     (ds-scale1-fx scale1-fx)
                     (ds-scale2-fx scale2-fx)
                     (light (ewd-sector-light near-sector)))

                ;; --- MIDDLE WALL (Transparent Grate) ---
                (when (and middle-texture far-sector)
                  (let ((drawseg (ern--masked-drawseg-new-from-freelist)))
                    (setf (ern--masked-drawseg-next drawseg) (ern--scene-masked-drawseg scene)
                          (ern--scene-masked-drawseg scene) drawseg
                          (ern--masked-drawseg-scene drawseg) scene
                          (ern--masked-drawseg-sector-light drawseg) (ewd-sector-light near-sector)
                          (ern--masked-drawseg-light-adjust drawseg) light-adjust
                          (ern--masked-drawseg-column-start drawseg) start-column
                          (ern--masked-drawseg-column-end drawseg) end-column
                          (ern--masked-drawseg-texture drawseg) middle-texture
                          (ern--masked-drawseg-near-floor drawseg) near-floor-height
                          (ern--masked-drawseg-far-floor drawseg) far-floor-height
                          (ern--masked-drawseg-near-ceiling drawseg) near-ceiling-height
                          (ern--masked-drawseg-far-ceiling drawseg) far-ceiling-height
                          (ern--masked-drawseg-upper-unpegged-p drawseg) upper-unpegged-p
                          (ern--masked-drawseg-lower-unpegged-p drawseg) lower-unpegged-p
                          (ern--masked-drawseg-scale-start drawseg) scale-start-fx
                          (ern--masked-drawseg-scale-step drawseg) scale-step-fx
                          (ern--masked-drawseg-rw-distance drawseg) rw-distance-fx
                          (ern--masked-drawseg-rw-offset drawseg) rw-offset-fx
                          (ern--masked-drawseg-rw-centerangle drawseg) rw-centerangle
                          (ern--masked-drawseg-texture-y-offset drawseg) texture-y-offset)
                    ;; Snapshot the exact clipping window available for this segment
                    (let ((snap-top (ern--masked-drawseg-clip-top-snapshot drawseg))
                          (snap-bot (ern--masked-drawseg-clip-bottom-snapshot drawseg)))
                      (cl-loop for c from start-column to end-column do
                               (if (aref clip-solid-vector c)
                                   ;; Force invalid intersection so it mathematically culls later
                                   (progn
                                     (aset snap-top c 1)
                                     (aset snap-bot c -1))
                                 (progn
                                   (aset snap-top c (aref clip-top-vector c))
                                   (aset snap-bot c (aref clip-bottom-vector c)))))))
                  (setq middle-texture nil))

                (while (<= column end-column)
                  (unless (aref clip-solid-vector column)
                    ;; Doom-style texture column: angle + constant
                    ;; perpendicular distance, no perspective division.
                    (let ((tex-x-fx (ern--texturecolumn-fx
                                     rw-offset-fx rw-distance-fx rw-centerangle
                                     (aref xtoviewangle column)))
                          (colormap (ern--find-drawseg-colormap scene (ewd-sector-light near-sector)
                                                                scale-start-fx light-adjust)))

                      (ern--draw-projected-column-clipped-fx
                       scene bitmap data-offset
                       column near-floor-height near-floor-texture near-ceiling-height near-ceiling-texture far-floor-height far-ceiling-height
                       far-sector middle-texture upper-texture
                       lower-texture tex-x-fx texture-y-offset colormap palette light
                       nc-y-fx nf-y-fx fc-y-fx ff-y-fx
                       upper-unpegged-p lower-unpegged-p view-z scale-start-fx)))

                  (setq column (1+ column)
                        scale-start-fx (+ scale-start-fx scale-step-fx)
                        nc-y-fx (+ nc-y-fx nc-y-step-fx)
                        nf-y-fx (+ nf-y-fx nf-y-step-fx)
                        fc-y-fx (when fc-y-fx (+ fc-y-fx fc-y-step-fx))
                        ff-y-fx (when ff-y-fx (+ ff-y-fx ff-y-step-fx))))
                (ern--store-drawseg scene start-column end-column ds-scale1-fx ds-scale2-fx wall-type)))))))))

(defsubst ern--seg-angle-delta (seg-x-fx seg-y-fx x-fx y-fx angle)
  "Signed BAM angle of a seg endpoint relative to the view ANGLE."
  (ern--bam-signed (eldoom-math-ang (- (ern--view-to-point-angle x-fx y-fx seg-x-fx seg-y-fx)
                                       angle))))

(defsubst ern--seg-visible-p (seg x-fx y-fx angle half-fov)
  (let* ((segment (ewd-seg-segment seg))
         (line-x1-fx (eldoom-math-segment-x1 segment))
         (line-y1-fx (eldoom-math-segment-y1 segment))
         (line-x2-fx (eldoom-math-segment-x2 segment))
         (line-y2-fx (eldoom-math-segment-y2 segment))
         (a1 (ern--seg-angle-delta line-x1-fx line-y1-fx x-fx y-fx angle))
         (a2 (ern--seg-angle-delta line-x2-fx line-y2-fx x-fx y-fx angle))
         (span (abs (- a1 a2)))
         (angle-min (min a1 a2))
         (angle-max (max a1 a2))
         (angle-wrap-p (> span eldoom-math-ang180)))
    (or angle-wrap-p
        (and (<= angle-min half-fov)
             (>= angle-max (- half-fov))))))

(defun ern--render-seg
    (scene bitmap data-offset
           seg x-fx y-fx angle view-z palette)
  (let* ((half-fov (ern--scene-half-fov scene))
         (near-sector (ewd-seg-near-sector seg))
         (far-sector (ewd-seg-far-sector seg))
         (viewer-sidedef (ewd-seg-near-sidedef seg)))
    (when (and (ern--seg-visible-p seg x-fx y-fx angle half-fov)
               near-sector
               (or (not far-sector)
                   (not (ewd-sidedef-middle viewer-sidedef))
                   (/= (ewd-sector-floor-height-fx near-sector)
                       (ewd-sector-floor-height-fx far-sector))
                   (/= (ewd-sector-ceiling-height-fx near-sector)
                       (ewd-sector-ceiling-height-fx far-sector))
                   (/= (ewd-sector-light near-sector)
                       (ewd-sector-light far-sector))
                   (not (eq (ewd-sector-floor-texture near-sector)
                            (ewd-sector-floor-texture far-sector)))
                   (not (eq (ewd-sector-ceiling-texture near-sector)
                            (ewd-sector-ceiling-texture far-sector)))))
      (ern--draw-candidate-projected
       scene bitmap data-offset
       seg near-sector far-sector viewer-sidedef
       view-z palette))))

(defun ern--render-subsector
    (scene bitmap data-offset
           subsector x-fx y-fx angle cos-fx sin-fx
           view-z palette)
  (let* ((segs (ewd-subsector-segs subsector)))
    (cl-loop for seg across segs do
             (ern--render-seg
              scene bitmap data-offset
              seg x-fx y-fx angle
              view-z palette))
    ;; Collect sprites immediately using the CURRENT clip window
    (when-let* ((sector (ewd-subsector-sector subsector))
                (thing-list-head (ewd-sector-thing sector))
                (s-thing (emo-sector-next thing-list-head)))
      (when (/= (ewd-sector-iteration sector) eldoom-globals-iteration)
        (setf (ewd-sector-iteration sector) eldoom-globals-iteration)
        (while s-thing
          (when (emo-sprite s-thing)
            (ern--try-queue-vissprite
             scene s-thing sector x-fx y-fx cos-fx sin-fx
             view-z))
          (setq s-thing (emo-sector-next s-thing)))))))

(defun ern--screen-solid-p (width)
  (= ern--solid-column-count width))

(defun ern--render-node
    (scene bitmap data-offset
           node x-fx y-fx angle cos-fx sin-fx
           view-z palette)
  (if (ewd-subsector-p node)
      (ern--render-subsector
       scene bitmap data-offset
       node
       x-fx y-fx angle cos-fx sin-fx view-z
       palette)
    (let* ((right-child-p (emp-point-in-right-child-p node x-fx y-fx))
           (front-child (if right-child-p
                            (ewd-node-right-child node)
                          (ewd-node-left-child node)))
           (back-child (if right-child-p
                           (ewd-node-left-child node)
                         (ewd-node-right-child node)))
           (back-bbox (if right-child-p
                          (ewd-node-left-bbox node)
                        (ewd-node-right-bbox node))))

      (ern--render-node
       scene bitmap data-offset
       front-child x-fx y-fx angle cos-fx sin-fx
       view-z palette)

      ;; 2. Only traverse the back side if its bounding box is within the FOV
      ;; AND if the screen isn't already completely blocked by solid walls.
      (unless (ern--screen-solid-p eldoom-canvas-width)
        (when (ern--bbox-visible-p back-bbox x-fx y-fx angle (ern--scene-half-fov scene))
          (ern--render-node
           scene bitmap data-offset
           back-child x-fx y-fx angle cos-fx sin-fx
           view-z palette))))))

(defun ern--draw-vissprite (vissprite drawseg bitmap data-offset palette)
  ;; (declare (speed 3) (safety 0))

  ;; Dynamically carve the visibility bounds ---
  (ern--clip-sprite-against-drawsegs vissprite drawseg eldoom-canvas-height)

  (let* ((x1 (max 0 (ern--vissprite-x1 vissprite)))
         (x2 (min (1- eldoom-canvas-width) (ern--vissprite-x2 vissprite)))
         (real-x1 (ern--vissprite-texture-x-fx vissprite))
         (scale-fx (ern--vissprite-scale-fx vissprite))
         (top-y-fx (ern--vissprite-texture-y-fx vissprite))
         (patch (ern--vissprite-patch vissprite))
         (colormap (ern--vissprite-colormap vissprite))
         (columns (ewd-patch-columns patch))
         (patch-width (ewd-patch-width patch)))

    (when (and (> scale-fx 0) columns (<= x1 x2))
      (let ((inv-scale-fx (eldoom-math-div eldoom-math-unit scale-fx)))
        (cl-loop for column from x1 to x2 do

                 ;; Read from our newly built dynamic arrays
                 (let ((clip-top-col (aref ern--sprite-clip-top column))
                       (clip-bottom-col (aref ern--sprite-clip-bottom column)))

                   (when (<= clip-top-col clip-bottom-col)
                     (let* ((patch-column-index
                             (min (1- patch-width)
                                  (max 0 (eldoom-math-fixed-to-int (* (- column real-x1) inv-scale-fx)))))
                            (patch-column (aref columns patch-column-index)))
                       (cl-loop for post across patch-column do
                                (let* ((post-y (ewd-patch-post-y-offset post))
                                       (data (ewd-patch-post-data post))
                                       (post-height (length data))
                                       (screen-y-start (eldoom-math-fixed-to-int (+ top-y-fx (* post-y scale-fx))))
                                       (screen-y-end (eldoom-math-fixed-to-int (+ top-y-fx (* (+ post-y post-height) scale-fx))))

                                       (draw-y-start (max clip-top-col screen-y-start))
                                       (draw-y-end (min clip-bottom-col (1- screen-y-end))))

                                  (when (<= draw-y-start draw-y-end)
                                    (ern--draw-column-span
                                     bitmap data-offset column draw-y-start draw-y-end
                                     data (* (- draw-y-start screen-y-start) inv-scale-fx) inv-scale-fx
                                     colormap palette post-height t))))))))))))

(defun ern--draw-masked-drawseg
    (drawseg bitmap data-offset view-z center-y-fx palette xtoviewangle)
  "Draw one queued transparent middle-texture wall segment."
  (let* ((scene (ern--masked-drawseg-scene drawseg))
         (texture (ern--masked-drawseg-texture drawseg))
         (sector-light (ern--masked-drawseg-sector-light drawseg))
         (light-adjust (ern--masked-drawseg-light-adjust drawseg))
         (start-column (ern--masked-drawseg-column-start drawseg))
         (end-column (ern--masked-drawseg-column-end drawseg))
         (near-floor (ern--masked-drawseg-near-floor drawseg))
         (far-floor (ern--masked-drawseg-far-floor drawseg))
         (near-ceiling (ern--masked-drawseg-near-ceiling drawseg))
         (far-ceiling (ern--masked-drawseg-far-ceiling drawseg))
         (upper-unpegged-p (ern--masked-drawseg-upper-unpegged-p drawseg))
         (lower-unpegged-p (ern--masked-drawseg-lower-unpegged-p drawseg))
         (tex-y-offset (ern--masked-drawseg-texture-y-offset drawseg))
         (snap-top (ern--masked-drawseg-clip-top-snapshot drawseg))
         (snap-bot (ern--masked-drawseg-clip-bottom-snapshot drawseg))
         (texture-width (ewd-texture-width texture))
         (texture-height (ewd-texture-height texture))
         (texture-columns (ewd-texture-columns texture))
         (scale (ern--masked-drawseg-scale-start drawseg))
         (scale-step (ern--masked-drawseg-scale-step drawseg))
         (rw-distance-fx (ern--masked-drawseg-rw-distance drawseg))
         (rw-offset-fx (ern--masked-drawseg-rw-offset drawseg))
         (rw-centerangle (ern--masked-drawseg-rw-centerangle drawseg)))
    (cl-loop for column from start-column to end-column do
             (let ((clip-top (aref snap-top column))
                   (clip-bottom (aref snap-bot column)))
               (when (<= clip-top clip-bottom)
                 (let* ((tex-x-fx (ern--texturecolumn-fx
                                   rw-offset-fx rw-distance-fx rw-centerangle
                                   (aref xtoviewangle column)))
                        (texture-column (aref texture-columns (mod (eldoom-math-fixed-to-int tex-x-fx) texture-width)))
                        (world-top-z (ern--texture-world-top-z 'middle-portal upper-unpegged-p lower-unpegged-p
                                                               near-ceiling near-floor far-ceiling far-floor
                                                               texture-height))
                        (world-bottom-z (- world-top-z texture-height))
                        (portal-top-z (min near-ceiling far-ceiling))
                        (portal-bottom-z (max near-floor far-floor))
                        ;; Screen Y = center + height * scale (integer * fixed).
                        (tex-top-y-fx (+ center-y-fx (* (- view-z world-top-z) scale)))
                        (tex-bottom-y-fx (+ center-y-fx (* (- view-z world-bottom-z) scale)))
                        (portal-top-y-fx (+ center-y-fx (* (- view-z portal-top-z) scale)))
                        (portal-bottom-y-fx (+ center-y-fx (* (- view-z portal-bottom-z) scale)))
                        (portal-clip-top (eldoom-math-fixed-ceil portal-top-y-fx))
                        (portal-clip-bottom (eldoom-math-fixed-to-int portal-bottom-y-fx))
                        (final-clip-top (max clip-top portal-clip-top))
                        (final-clip-bottom (min clip-bottom portal-clip-bottom))
                        (draw-start (max final-clip-top (eldoom-math-fixed-ceil tex-top-y-fx)))
                        (draw-end (min final-clip-bottom (eldoom-math-fixed-to-int tex-bottom-y-fx)))
                        (colormap (ern--find-drawseg-colormap scene sector-light scale light-adjust)))
                   (when (<= draw-start draw-end)
                     (let* ((denom-fx (max 1 (- tex-bottom-y-fx tex-top-y-fx)))
                            (step-fx (eldoom-math-div (eldoom-math-fixed-from-int texture-height) denom-fx))
                            (tex-y-fx (+ (eldoom-math-fixed-from-int tex-y-offset)
                                         (eldoom-math-fixed-to-int
                                          (* (- (eldoom-math-fixed-from-int draw-start) tex-top-y-fx)
                                             step-fx)))))
                       (ern--draw-column-span
                        bitmap data-offset column draw-start draw-end
                        texture-column tex-y-fx step-fx colormap palette texture-height
                        t))))))
             (setq scale (+ scale scale-step)))))

(cl-defstruct (ern--draw-item
               (:constructor ern--make-draw-item)
               (:copier nil))
  (scale-fx 0 :type fixnum)  ;; Smaller scale = further away (drawn first)
  obj                        ;; The underlying vissprite or masked-drawseg struct
  next)

(define-eldoom-intrusive-freelist ern--draw-item
                                  ern--draw-item-next
                                  ern--make-draw-item)

(defun ern--clear-draw-items (scene)
  "Moves all draw-items from draw-item list to freelist"
  (let ((draw-item (ern--scene-draw-item scene)))
    (while draw-item
      (let ((draw-item-next (ern--draw-item-next draw-item)))
        (ern--draw-item-move-to-freelist draw-item)
        (setq draw-item draw-item-next))))
  (setf (ern--scene-draw-item scene) nil))

(defun ern--split-draw-items (head)
  (let ((slow head)
        (fast head)
        (prev nil))
    (while (and fast (ern--draw-item-next fast))
      (setq prev slow)
      (setq slow (ern--draw-item-next slow))
      (setq fast (ern--draw-item-next (ern--draw-item-next fast))))
    (when prev
      (setf (ern--draw-item-next prev) nil))
    slow))

(defun ern--merge-draw-items (head1 head2)
  (cond
   ((null head1) head2)
   ((null head2) head1)
   (t (let* ((head
              (if (< (ern--draw-item-scale-fx head1) (ern--draw-item-scale-fx head2))
                  (prog1 head1 (setq head1 (ern--draw-item-next head1)))
                (prog1 head2 (setq head2 (ern--draw-item-next head2)))))
             (tail head))
        (while (and head1 head2)
          (if (< (ern--draw-item-scale-fx head1) (ern--draw-item-scale-fx head2))
              (setf (ern--draw-item-next tail) head1
                    tail head1
                    head1 (ern--draw-item-next head1))
            (setf (ern--draw-item-next tail) head2
                  tail head2
                  head2 (ern--draw-item-next head2))))
        (setf (ern--draw-item-next tail) (or head1 head2))
        head))))

(defun ern--merge-sort-draw-items (head)
  (if (or (null head) (null (ern--draw-item-next head)))
      head
    (let* ((right (ern--split-draw-items head)))
      (ern--merge-draw-items
       (ern--merge-sort-draw-items head)
       (ern--merge-sort-draw-items right)))))

(defun ern--draw-transparent-queue
    (scene bitmap data-offset palette view-z)
  "Depth-sort and draw sprites together with transparent middle textures."
  (let* ((masked-drawseg (ern--scene-masked-drawseg scene))
         (drawseg (ern--scene-drawseg scene))
         (vissprite (ern--scene-vissprite scene))
         (center-y-fx (ern--scene-center-y scene))
         (xtoviewangle (ern--scene-xtoviewangle scene)))
    (while masked-drawseg
      (let ((item (ern--draw-item-new-from-freelist)))
        (setf (ern--draw-item-scale-fx item)
              ;; Already projection/depth in fixed-point, matching vissprite scale.
              (ern--masked-drawseg-scale-start masked-drawseg)
              (ern--draw-item-obj item) masked-drawseg
              masked-drawseg (ern--masked-drawseg-next masked-drawseg)
              (ern--draw-item-next item) (ern--scene-draw-item scene)
              (ern--scene-draw-item scene) item)))

    (while vissprite
      (let ((item (ern--draw-item-new-from-freelist)))
        (setf (ern--draw-item-scale-fx item)
              (ern--vissprite-scale-fx vissprite)
              (ern--draw-item-obj item) vissprite
              vissprite (ern--vissprite-next vissprite)
              (ern--draw-item-next item) (ern--scene-draw-item scene)
              (ern--scene-draw-item scene) item)))
    (setf (ern--scene-draw-item scene)
          (ern--merge-sort-draw-items (ern--scene-draw-item scene)))
    (let ((item (ern--scene-draw-item scene)))
      (while item
        (let ((item-obj (ern--draw-item-obj item)))
          (if (ern--masked-drawseg-p item-obj)
              (ern--draw-masked-drawseg
               item-obj bitmap
               data-offset view-z center-y-fx palette xtoviewangle)
            (ern--draw-vissprite
             item-obj drawseg bitmap data-offset palette))
          (setq item (ern--draw-item-next item)))))))

(defun ern-render-level
    (map scene canvas x-fx y-fx z-fx angle-bam fixed-colormap extra-light)
  "Render MAP onto CANVAS using Doom's BSP strictly front-to-back."
  (declare (ftype (function (t t array fixnum fixnum fixnum fixnum t fixnum) t)))
  (let* ((width eldoom-canvas-width)
         (height eldoom-canvas-height)
         (bitmap (eldoom-canvas-bitmap canvas))
         (data-offset (eldoom-canvas-data-offset canvas))
         (cos-fx (eldoom-math-cos angle-bam))
         (sin-fx (eldoom-math-sin angle-bam))
         (root-node (ewd-map-root-node map))
         (z (eldoom-math-fixed-to-int z-fx)))
    (cl-incf eldoom-globals-iteration)
    (ern--clear-clips scene width height)
    (ern--clear-visplanes scene)
    (ern--clear-masked-drawsegs scene)
    (ern--clear-vissprites scene)
    (ern--clear-drawsegs scene)
    (ern--clear-draw-items scene)
    (setf (ern--scene-fixed-colormap scene) fixed-colormap
          (ern--scene-extra-light scene) extra-light)
    (eldoom-math-set-view-transformation (ern--scene-view-transformation scene)
                                         x-fx y-fx z-fx
                                         angle-bam)
    (let* ((palettes (ewd-map-palettes map))
           (palette (aref palettes 0)))
      (ern--render-node
       scene bitmap data-offset
       root-node
       x-fx y-fx angle-bam cos-fx sin-fx
       z palette)
      (ern--draw-visplanes
       scene bitmap data-offset palette
       x-fx y-fx angle-bam cos-fx sin-fx z)
      (ern--draw-transparent-queue
       scene bitmap data-offset palette
       z))
    canvas))

;; Local Variables:
;; read-symbol-shorthands: (("ern-" . "eldoom-render-") ("ewd-" . "eldoom-wad-") ("emo-" . "eldoom-mobj-") ("enf-" . "eldoom-info-") ("emp-" . "eldoom-map-"))
;; End:
(provide 'eldoom-render)
;;; eldoom-render.el ends here
