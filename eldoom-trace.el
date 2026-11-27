;;; eldoom-trace.el --- Doom line traces and hitscan -*- lexical-binding:t -*-

;; Copyright (C) 2026 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; Path traversal and hitscan modelled on vanilla p_maputl.c / p_map.c
;; (P_PathTraverse, PIT_Add*Intercepts, PTR_ShootTraverse, P_LineAttack).

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'eldoom-info)
(eval-when-compile (require 'eldoom-intrusive))
(require 'eldoom-map)
(require 'eldoom-math)
(require 'eldoom-mobj)
(require 'eldoom-sight)
(require 'eldoom-wad)

(defconst etr-pt-addlines 1)
(defconst etr-pt-addthings 2)
(defconst etr--mapblocksize 128)
(defconst etr--mapblockshift 7)

(cl-defstruct (etr-hit
               (:constructor etr-make-hit) (:copier nil))
  (frac 0 :type fixnum)
  (x 0 :type fixnum)
  (y 0 :type fixnum)
  (z 0 :type fixnum)
  linedef
  mobj
  (kind nil))

(cl-defstruct (etr-tracer
               (:constructor etr--make-tracer) (:copier nil))
  segment
  (z-fx 0 :type fixnum)
  (top-slope 0 :type fixnum)
  (bottom-slope 0 :type fixnum)
  intercepts)

(defvar-local etr--tracer nil)
(defun etr-init ()
  (setq etr--tracer (etr--make-tracer))
  (setf (etr-tracer-segment etr--tracer) (eldoom-math-make-segment)))

(cl-defstruct (etr-intercept (:constructor etr--make-intercept) (:copier nil))
  frac item next)

(define-eldoom-intrusive-freelist eldoom-trace--intercept
                                  eldoom-trace-intercept-next
                                  eldoom-trace--make-intercept)

(defun etr--split-intercepts (head)
  (let ((slow head)
        (fast head)
        (prev nil))
    (while (and fast (etr-intercept-next fast))
      (setq prev slow)
      (setq slow (etr-intercept-next slow))
      (setq fast (etr-intercept-next (etr-intercept-next fast))))

    (when prev
      (setf (etr-intercept-next prev) nil))

    slow))

(defun etr--merge-intercepts (head1 head2)
  (cond
   ((null head1) head2)
   ((null head2) head1)
   (t (let* ((head
              (if (< (etr-intercept-frac head1) (etr-intercept-frac head2))
                  (prog1 head1 (setq head1 (etr-intercept-next head1)))
                (prog1 head2 (setq head2 (etr-intercept-next head2)))))
             (tail head))
        (while (and head1 head2)
          (if (< (etr-intercept-frac head1) (etr-intercept-frac head2))
              (setf (etr-intercept-next tail) head1
                    tail head1
                    head1 (etr-intercept-next head1))
            (setf (etr-intercept-next tail) head2
                  tail head2
                  head2 (etr-intercept-next head2))))
        (setf (etr-intercept-next tail) (or head1 head2))
        head))))

(defun etr--merge-sort-intercepts (head)
  (if (or (null head) (null (etr-intercept-next head)))
      head
    (let* ((right (etr--split-intercepts head)))
      (etr--merge-intercepts
       (etr--merge-sort-intercepts head)
       (etr--merge-sort-intercepts right)))))

(defun etr--free-intercepts ()
  (let ((intercept (etr-tracer-intercepts etr--tracer)))
    (while intercept
      (let ((intercept-next (etr-intercept-next intercept)))
        (etr--intercept-move-to-freelist intercept)
        (setq intercept intercept-next))))
  (setf (etr-tracer-intercepts etr--tracer) nil))

(defun etr--add-intercept (frac item)
  (let ((intercept (etr--intercept-new-from-freelist)))
    (setf (etr-intercept-frac intercept) frac
          (etr-intercept-item intercept) item
          (etr-intercept-next intercept) (etr-tracer-intercepts etr--tracer)
          (etr-tracer-intercepts etr--tracer) intercept)))

(defun etr--add-line-intercepts (linedef)
  "PIT_AddLineIntercepts in p_maputl.c
If appropriate add line to list of intercepts.
Returns non nil if we should stop checking"
  (let* ((trace-seg (etr-tracer-segment etr--tracer))
         (x1 (eldoom-math-segment-x1 trace-seg))
         (y1 (eldoom-math-segment-y1 trace-seg))
         (x2 (eldoom-math-segment-x2 trace-seg))
         (y2 (eldoom-math-segment-y2 trace-seg))
         (dx (- x2 x1))
         (dy (- y2 y1))
         (line-seg (ewd-linedef-segment linedef))
         (s1 nil) ;; placeholder
         (s2 nil)) ;; placeholder
    (if (or (> dx (eldoom-math-fixed-from-int 16))
            (> dy (eldoom-math-fixed-from-int 16))
            (< dx (eldoom-math-fixed-from-int -16))
            (< dy (eldoom-math-fixed-from-int -16))) ;; avoids precision problems (comment from the original code)
        (setq s1 (eldoom-math-on-right-side-p (eldoom-math-segment-x1 line-seg) (eldoom-math-segment-y1 line-seg) trace-seg)
              s2 (eldoom-math-on-right-side-p (eldoom-math-segment-x2 line-seg) (eldoom-math-segment-y2 line-seg) trace-seg))
      (setq s1 (eldoom-math-on-right-side-p x1 y1 line-seg)
            s2 (eldoom-math-on-right-side-p x2 y2 line-seg)))
    (when (not (eql s1 s2))
      (let ((frac (eldoom-math-ray-intersection-fraction trace-seg line-seg)))
        (when (>= frac 0)
          (etr--add-intercept frac linedef))))))

(defvar-local etr--mobj-bbox nil)
(defvar-local etr--mobj-segment nil)
(defun etr--add-thing-intercepts (mobj)
  "PIT_AddThingIntercepts in p_maputl.c"
  (when (not etr--mobj-bbox) (setf etr--mobj-bbox (eldoom-math-make-bbox)))
  (when (not etr--mobj-segment) (setf etr--mobj-segment (eldoom-math-make-segment)))
  (let* ((mobj-radius-fx (emo-radius-fx mobj))
         (mobj-x-fx (emo-x-fx mobj))
         (mobj-y-fx (emo-y-fx mobj))
         (mobj-x1-fx (- mobj-x-fx mobj-radius-fx))
         (mobj-y1-fx (- mobj-y-fx mobj-radius-fx))
         (mobj-x2-fx (+ mobj-x-fx mobj-radius-fx))
         (mobj-y2-fx (+ mobj-y-fx mobj-radius-fx)))
    (setf (eldoom-math-bbox-left etr--mobj-bbox) mobj-x1-fx
          (eldoom-math-bbox-right etr--mobj-bbox) mobj-x2-fx
          (eldoom-math-bbox-bottom etr--mobj-bbox) mobj-y1-fx
          (eldoom-math-bbox-top etr--mobj-bbox) mobj-y2-fx)
    (when (eldoom-math-line-intersects-bbox-p (etr-tracer-segment etr--tracer) etr--mobj-bbox)
      (setf (eldoom-math-segment-x1 etr--mobj-segment) mobj-x1-fx
            (eldoom-math-segment-y1 etr--mobj-segment) mobj-y1-fx
            (eldoom-math-segment-x2 etr--mobj-segment) mobj-x2-fx
            (eldoom-math-segment-y2 etr--mobj-segment) mobj-y2-fx)
      (let ((frac (eldoom-math-ray-intersection-fraction (etr-tracer-segment etr--tracer) etr--mobj-segment)))
        (when (>= frac 0)
          (etr--add-intercept frac mobj))))))

(defun etr--traverse-intercepts (traverse obj)
  "P_TraverseIntercepts in p_maputl.c"
  (setf (etr-tracer-intercepts etr--tracer) (etr--merge-sort-intercepts (etr-tracer-intercepts etr--tracer)))
  (let ((intercept (etr-tracer-intercepts etr--tracer)))
    (while (and intercept (<= (etr-intercept-frac intercept) eldoom-math-unit))
      (funcall traverse intercept obj)
      (setf intercept (etr-intercept-next intercept)))))

(defun etr-path-traverse (x1 y1 x2 y2 flags traverse obj)
  "P_PathTraverse in p_maputl.c"
  (cl-incf eldoom-globals-iteration)
  (let* ((blockmap (ewd-map-blockmap ewd-current-map))
         (origin-x (ewd-blockmap-origin-x-fx blockmap))
         (origin-y (ewd-blockmap-origin-y-fx blockmap)))
    (when (zerop (logand (- x1 origin-x) (1- emp-block-size)))
      (cl-incf x1 eldoom-math-unit))
    (when (zerop (logand (- y1 origin-y) (1- emp-block-size)))
      (cl-incf y1 eldoom-math-unit))
    (let* ((dx (- x2 x1))
           (dy (- y2 y1)))
      (setf (eldoom-math-segment-x1 (etr-tracer-segment etr--tracer)) x1
            (eldoom-math-segment-y1 (etr-tracer-segment etr--tracer)) y1
            (eldoom-math-segment-x2 (etr-tracer-segment etr--tracer)) x2
            (eldoom-math-segment-y2 (etr-tracer-segment etr--tracer)) y2
            (etr-tracer-z-fx etr--tracer) 0)
      (etr--free-intercepts)
      (cl-decf x1 origin-x)
      (cl-decf x2 origin-x)
      (cl-decf y1 origin-y)
      (cl-decf y2 origin-y)
      (let ((xt1 (ash x1 ewd-map-block-shift))
            (yt1 (ash y1 ewd-map-block-shift))
            (xt2 (ash x2 ewd-map-block-shift))
            (yt2 (ash y2 ewd-map-block-shift))
            (map-x-step 0)
            (map-y-step 0)
            (x-step (eldoom-math-fixed-from-int 256))
            (y-step (eldoom-math-fixed-from-int 256))
            (x-partial eldoom-math-unit)
            (y-partial eldoom-math-unit))
        (cond
         ((> xt2 xt1)
          (setq map-x-step 1
                y-partial (- eldoom-math-unit (logand (ash x1 ewd-map-block-to-fixed) (1- eldoom-math-unit)))
                y-step (eldoom-math-div dy (abs dx))))
         ((< xt2 xt1)
          (setq map-x-step -1
                y-partial (logand (ash x1 ewd-map-block-to-fixed) (1- eldoom-math-unit))
                y-step (eldoom-math-div dy (abs dx)))))
        (cond
         ((> yt2 yt1)
          (setq map-y-step 1
                x-partial (- eldoom-math-unit (logand (ash y1 ewd-map-block-to-fixed) (1- eldoom-math-unit)))
                x-step (eldoom-math-div dx (abs dy))))
         ((< yt2 yt1)
          (setq map-y-step -1
                x-partial (logand (ash y1 ewd-map-block-to-fixed) (1- eldoom-math-unit))
                x-step (eldoom-math-div dx (abs dy)))))
        (let ((y-intercept (+ (ash y1 ewd-map-block-to-fixed) (eldoom-math-mul y-partial y-step)))
              (x-intercept (+ (ash x1 ewd-map-block-to-fixed) (eldoom-math-mul x-partial x-step))))
          (catch :break
            (cl-loop repeat 64 do
                     (when (eldoom-math-has-flag flags etr-pt-addlines)
                       (emp-for-each-line-in-block blockmap xt1 yt1 #'etr--add-line-intercepts))
                     (when (eldoom-math-has-flag flags etr-pt-addthings)
                       (emo-for-each-in-block mobj xt1 yt1 do
                                              (etr--add-thing-intercepts mobj)))
                     (when (and (= xt1 xt2) (= yt1 yt2))
                       (throw :break t))
                     (cond
                      ((= (eldoom-math-fixed-to-int y-intercept) yt1)
                       (cl-incf y-intercept y-step)
                       (cl-incf xt1 map-x-step))
                      ((= (eldoom-math-fixed-to-int x-intercept) xt1)
                       (cl-incf x-intercept x-step)
                       (cl-incf yt1 map-y-step))))))
        (etr--traverse-intercepts traverse obj)))))

(cl-defstruct (etr--aim-state (:constructor etr--make-aim-state) (:copier nil))
  shooter
  (shoot-z 0 :type fixnum)
  (top-slope 0 :type fixnum)
  (bottom-slope 0 :type fixnum)
  (attack-range 0 :type fixnum)
  target)

(defun etr--aim-traverse (intercept aim-state)
  "PTR_AimTraverse in p_map.c"
  (let ((shoot-z (etr--aim-state-shoot-z aim-state))
        (attack-range (etr--aim-state-attack-range aim-state)))
    (if (ewd-linedef-p (etr-intercept-item intercept))
        (let* ((line (etr-intercept-item intercept)))
          (when (not (eldoom-math-has-flag (ewd-linedef-flags line) enf-ML_TWOSIDED))
            (throw :aim-slope nil))
          (let* ((front (ewd-linedef-front-sector line))
                 (front-floor (ewd-sector-floor-height-fx front))
                 (front-ceiling (ewd-sector-ceiling-height-fx front))
                 (back (ewd-linedef-back-sector line))
                 (back-floor (ewd-sector-floor-height-fx back))
                 (back-ceiling (ewd-sector-ceiling-height-fx back)))
            (when (or (/= front-floor back-floor) (/= front-ceiling back-ceiling))
              (let ((open-top (min front-ceiling back-ceiling))
                    (open-bottom (max front-floor back-floor)))
                (when (>= open-bottom open-top)
                  (throw :aim-slope nil))
                (let ((dist (eldoom-math-mul attack-range
                                             (etr-intercept-frac intercept)))
                      (slope-bottom (etr--aim-state-bottom-slope aim-state))
                      (slope-top (etr--aim-state-top-slope aim-state)))
                  (when (/= front-floor back-floor)
                    (let ((slope (eldoom-math-div (- open-bottom shoot-z) dist)))
                      (setf slope-bottom (max slope-bottom slope))))
                  (when (/= front-ceiling back-ceiling)
                    (let ((slope (eldoom-math-div (- open-top shoot-z) dist)))
                      (setf slope-top (min slope-top slope))))
                  (when (<= slope-top slope-bottom)
                    (throw :aim-slope nil))
                  (setf (etr--aim-state-bottom-slope aim-state) slope-bottom
                        (etr--aim-state-top-slope aim-state) slope-top))))))
      (let ((thing (etr-intercept-item intercept)))
        (when (and (not (eql thing (etr--aim-state-shooter aim-state)))
                   (eldoom-math-has-flag (emo-flags thing) enf-MF_SHOOTABLE))
          (let* ((dist (eldoom-math-mul attack-range (etr-intercept-frac intercept)))
                 (thing-bottom (emo-z-fx thing))
                 (thing-top (+ thing-bottom (emo-height-fx thing)))
                 (thing-top-slope (eldoom-math-div (- thing-top shoot-z) dist))
                 (thing-bottom-slope (eldoom-math-div (- thing-bottom shoot-z) dist)))
            (when (and (>= thing-top-slope (etr--aim-state-bottom-slope aim-state))
                       (<= thing-bottom-slope (etr--aim-state-top-slope aim-state))) ;; shooting into the thing
              (setq thing-top-slope (min thing-top-slope (etr--aim-state-top-slope aim-state))
                    thing-bottom-slope (max thing-bottom-slope (etr--aim-state-bottom-slope aim-state)))
              (setf (etr--aim-state-target aim-state) thing)
              (throw :aim-slope (/ (+ thing-top-slope thing-bottom-slope) 2)))))))))

(defvar-local etr--aim-state nil)
(cl-defstruct (etr-aim-result (:constructor etr--make-aim-result) (:copier nil))
  (slope 0 :type fixnum)
  target)

(defvar-local etr--aim-result nil)
(defun etr-aim-line-attack (mobj angle distance)
  "P_AimLineAttack in p_map.c"
  (when (not etr--aim-state) (setq etr--aim-state (etr--make-aim-state)))
  (when (not etr--aim-result) (setq etr--aim-result (etr--make-aim-result)))
  (let ((x2 (+ (emo-x-fx mobj) (eldoom-math-mul distance (eldoom-math-cos angle))))
        (y2 (+ (emo-y-fx mobj) (eldoom-math-mul distance (eldoom-math-sin angle)))))
    (setf (etr--aim-state-shooter etr--aim-state) mobj
          (etr--aim-state-shoot-z etr--aim-state)
          (+ (emo-z-fx mobj) (/ (emo-height-fx mobj) 2) (eldoom-math-fixed-from-int 8))
          (etr--aim-state-top-slope etr--aim-state) (/ (eldoom-math-fixed-from-int 100) 160)
          (etr--aim-state-bottom-slope etr--aim-state) (/ (eldoom-math-fixed-from-int -100) 160)
          (etr--aim-state-attack-range etr--aim-state) distance
          (etr--aim-state-target etr--aim-state) nil)
    (let ((slope (or (catch :aim-slope
                       (etr-path-traverse (emo-x-fx mobj) (emo-y-fx mobj) x2 y2
                                          (logior etr-pt-addlines etr-pt-addthings)
                                          #'etr--aim-traverse etr--aim-state))
                     0)))
      (setf (etr-aim-result-slope etr--aim-result) slope
            (etr-aim-result-target etr--aim-result) (etr--aim-state-target etr--aim-state))
      etr--aim-result)))

(defun etr--shoot-special-line (shooter line)
  "P_ShootSpecialLine in p_spec.c"
  (when-let* ((on-shoot (ewd-linedef-on-shoot line)))
    (funcall on-shoot shooter line)))

(cl-defstruct (etr--shoot-state (:constructor etr--make-shoot-state) (:copier nil))
  shooter
  (shoot-z 0 :type fixnum)
  (attack-range 0 :type fixnum)
  (aim-slope 0 :type fixnum))

(defun etr--shoot-traverse (intercept shoot-state)
  "PTR_ShootTraverse in p_map.c"
  (if (ewd-linedef-p (etr-intercept-item intercept))
      (let ((line (etr-intercept-item intercept))
            (attack-range (etr--shoot-state-attack-range shoot-state))
            (aim-slope (etr--shoot-state-aim-slope shoot-state))
            (shoot-z (etr--shoot-state-shoot-z shoot-state)))
        (catch :no-hit
          (when (/= 0 (ewd-linedef-special line))
            (etr--shoot-special-line (etr--shoot-state-shooter shoot-state) line))
          (when (eldoom-math-has-flag (ewd-linedef-flags line) enf-ML_TWOSIDED)
            (let* ((front (ewd-linedef-front-sector line))
                   (front-floor (ewd-sector-floor-height-fx front))
                   (front-ceiling (ewd-sector-ceiling-height-fx front))
                   (back (ewd-linedef-back-sector line))
                   (back-floor (ewd-sector-floor-height-fx back))
                   (back-ceiling (ewd-sector-ceiling-height-fx back)))
              (if (and (= front-floor back-floor) (= front-ceiling back-ceiling))
                  (throw :no-hit t)
                (let ((open-top (min front-ceiling back-ceiling))
                      (open-bottom (max front-floor back-floor))
                      (dist (eldoom-math-mul attack-range (etr-intercept-frac intercept))))
                  (when (and (or (= front-floor back-floor)
                                 (<= (eldoom-math-div (- open-bottom shoot-z) dist)
                                     aim-slope))
                             (or (= front-ceiling back-ceiling)
                                 (>= (eldoom-math-div (- open-top shoot-z) dist)
                                     aim-slope)))
                    (throw :no-hit t))))))
          (throw :hit intercept)))
    (let ((mobj (etr-intercept-item intercept))
          (attack-range (etr--shoot-state-attack-range shoot-state))
          (aim-slope (etr--shoot-state-aim-slope shoot-state))
          (shoot-z (etr--shoot-state-shoot-z shoot-state)))
      (when (and (not (eql mobj (etr--shoot-state-shooter shoot-state))) ;; can't shoot self
                 (eldoom-math-has-flag (emo-flags mobj) enf-MF_SHOOTABLE))
        (let* ((dist (eldoom-math-mul attack-range (etr-intercept-frac intercept)))
               (top-slope (eldoom-math-div (- (+ (emo-z-fx mobj) (emo-height-fx mobj)) shoot-z) dist))
               (bottom-slope (eldoom-math-div (- (emo-z-fx mobj) shoot-z) dist)))
          (when (<= bottom-slope aim-slope top-slope)
            (throw :hit intercept)))))))

(defvar-local etr--shoot-state nil)
(defun etr-shoot (mobj angle distance slope damage)
  "When a thing is shot return it
P_LineAttack in p_map.c"
  (let* ((x1 (emo-x-fx mobj))
         (y1 (emo-y-fx mobj))
         (dx (eldoom-math-mul distance (eldoom-math-cos angle)))
         (dy (eldoom-math-mul distance (eldoom-math-sin angle)))
         (z1 (+ (emo-z-fx mobj) (/ (emo-height-fx mobj) 2)
                (eldoom-math-fixed-from-int 8))))
    (when (not etr--shoot-state) (setq etr--shoot-state (etr--make-shoot-state)))
    (setf (etr--shoot-state-shooter etr--shoot-state) mobj
          (etr--shoot-state-shoot-z etr--shoot-state) z1
          (etr--shoot-state-attack-range etr--shoot-state) distance
          (etr--shoot-state-aim-slope etr--shoot-state) slope)
    (when-let*
        ((intercept (catch :hit
                      (etr-path-traverse x1 y1 (+ x1 dx) (+ y1 dy)
                                         (logior etr-pt-addlines etr-pt-addthings)
                                         #'etr--shoot-traverse etr--shoot-state))))
      (if (ewd-linedef-p (etr-intercept-item intercept))
          (let* ((frac (- (etr-intercept-frac intercept)
                          (eldoom-math-div (eldoom-math-fixed-from-int 4) distance)))
                 (x (+ x1 (eldoom-math-mul dx frac)))
                 (y (+ y1 (eldoom-math-mul dy frac)))
                 (z (+ z1 (eldoom-math-mul slope (eldoom-math-mul frac distance)))))
            ;; TODO: handle shooting at the sky
            (emo-spawn-puff x y z (= distance eldoom-globals-melee-range))
            nil)
        (let* ((target (etr-intercept-item intercept))
               (frac (- (etr-intercept-frac intercept)
                        (eldoom-math-div (eldoom-math-fixed-from-int 10) distance)))
               (x (+ x1 (eldoom-math-mul dx frac)))
               (y (+ y1 (eldoom-math-mul dy frac)))
               (z (+ z1 (eldoom-math-mul slope (eldoom-math-mul frac distance)))))
          (if (eldoom-math-has-flag (emo-flags mobj) enf-MF_NOBLOOD)
              (emo-spawn-puff x y z (= distance eldoom-globals-melee-range))
            (emo-spawn-blood x y z damage))
          (when (/= 0 damage)
            (emo-damage target mobj mobj damage))
          target)))))

(defun etr--radius-attack-callback (mobj spot source damage)
  "PIT_RadiusAttack in p_map.c"
  (when (and (eldoom-math-has-flag (emo-flags mobj) enf-MF_SHOOTABLE)
             (/= (emo-type mobj) enf-MT_CYBORG)
             (/= (emo-type mobj) enf-MT_SPIDER))
    (let* ((dx (abs (- (emo-x-fx mobj) (emo-x-fx spot))))
           (dy (abs (- (emo-y-fx mobj) (emo-y-fx spot))))
           (dist (max (eldoom-math-fixed-to-int (- (max dx dy) (emo-radius-fx mobj)))
                      0)))
      (when (and (< dist damage)
                 (eldoom-sight-check mobj spot))
        (emo-damage mobj spot source (- damage dist))))))

(defun etr-radius-attack (spot source damage)
  "P_RadiusAttack in p_map.c"
  (let* ((dist (eldoom-math-fixed-from-int damage)))
    (emo-for-each-in-block-overlapping-bbox
        mobj (- (emo-x-fx spot) dist) (+ (emo-x-fx spot) dist)
        (- (emo-y-fx spot) dist) (+ (emo-y-fx spot) dist) do
        (etr--radius-attack-callback mobj spot source damage))))

;; Local Variables:
;; read-symbol-shorthands: (("etr-" . "eldoom-trace-") ("emo-" . "eldoom-mobj-") ("emp-" . "eldoom-map-") ("ewd-" . "eldoom-wad-") ("enf-" . "eldoom-info-"))
;; End:
(provide 'eldoom-trace)
;;; eldoom-trace.el ends here
