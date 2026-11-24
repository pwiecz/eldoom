;;; eldoom-math.el --- Math utils -*- lexical-binding:t -*-

;; Copyright (C) 2026 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))

;;; Commentary:

;; Fixed-point arithmetic and trigonometry, geometry.
;;
;; Using similar fixed-point math as in the Doom engine helps avoid
;; memory allocations which Emacs Lisp interpreter is performing
;; for each floating point arithmetic operation.
;; Fixed point math requires 64-bit cpu architecture.
;; Emacs fixnums are 62-bit, so 16.16 values fit, but the 64-bit product of
;; two large fixed values would overflow into a (slow, allocating) bignum.
;; `eldoom-math-mul' therefore splits the operands into 16-bit halves like the
;; original FixedMul rather than multiplying directly.

;;; Code:

(eval-when-compile (require 'cl-lib))
(eval-when-compile (require 'inline))
(require 'gv)

(defconst eldoom-math-bits 16
  "Number of fractional bits in a fixed-point value (Doom FRACBITS).")
(defconst eldoom-math-unit 65536
  "Fixed-point representation of 1.0 (Doom FRACUNIT).")
(defconst eldoom-math-unit-f 65536.0)
(defconst eldoom-math-int-max 2147483647)
(defconst eldoom-math-int-min -2147483648)

;; --- BAM angles (a full circle is 2^32) -------------------------------------

(defconst eldoom-math-ang45    #x20000000)
(defconst eldoom-math-ang90    #x40000000)
(defconst eldoom-math-ang180   #x80000000)
(defconst eldoom-math-ang270   #xC0000000)
(defconst eldoom-math-ang-max #x100000000 "One full turn (2^32); angles are taken modulo this.")
(defconst eldoom-math-ang-mask #xFFFFFFFF)

(defconst eldoom-math-fine-angles 8192 "Number of entries in a quarter+full fine table (FINEANGLES).")
(defconst eldoom-math-fine-mask 8191)

(defconst eldoom-math-fine-tan-mask 4095)

(defconst eldoom-math-angle-to-fine-shift -19
  "Shift a 32-bit BAM angle right by this to index the fine tables.")

(defconst eldoom-math-angle-from-fine-shift 19
  "Shift a fine table index angle left by this to obtain a 32-bit BAM.")

(defconst eldoom-math-slope-bits 11)
(defconst eldoom-math-slope-range 2048 "SLOPERANGE: domain of `tantoangle'.")
(defconst eldoom-math-slope-mask 2047)

(define-inline eldoom-math-ang (a)
  "Wrap integer A into the unsigned 32-bit BAM range."
  (declare (ftype (function (fixnum) fixnum)) (pure t))
  (inline-letevals (a)
    (inline-quote (logand ,a eldoom-math-ang-mask))))

(defmacro eldoom-math-angle-incf (angle delta)
  "Increment ANGLE by DELTA using wrap around arithmetic so that the resulting
angle is between 0 and eldoom-math-and-max."
  (gv-letplace (getter setter) angle
    (funcall setter `(logand (+ ,getter ,delta) eldoom-math-ang-mask))))

(defmacro eldoom-math-angle-decf (angle delta)
  "Decrement ANGLE by DELTA using wrap around arithmetic so that the resulting
angle is between 0 and eldoom-math-and-max."
  (gv-letplace (getter setter) angle
    (funcall setter `(logand (- ,getter ,delta) eldoom-math-ang-mask))))

(define-inline eldoom-math-angle-add (ang1 ang2)
  "Add ANG2 to ANG1 using wrap around arithmetic so that the resulting
angle is between 0 and eldoom-math-and-max"
  (declare (ftype (function (fixnum fixnum) fixnum)) (pure t))
  (inline-letevals (ang1 ang2)
    (inline-quote (eldoom-math-ang (+ ,ang1 ,ang2)))))

(define-inline eldoom-math-angle-sub (ang1 ang2)
  "Subtract ANG2 from ANG1 using wrap around arithmetic so that the resulting
angle is between 0 and eldoom-math-and-max"
  (declare (ftype (function (fixnum fixnum) fixnum)) (pure t))
  (inline-letevals (ang1 ang2)
    (inline-quote (eldoom-math-ang (- ,ang1 ,ang2)))))

;; --- Fixed-point arithmetic -------------------------------------------------

(define-inline eldoom-math-fixed-from-int (x)
  "Convert integer X to fixed-point."
  (declare (ftype (function (fixnum) fixnum)) (pure t))
  (inline-letevals (x)
    (inline-quote (ash ,x eldoom-math-bits))))

(define-inline eldoom-math-fixed-to-int (x)
  "Truncate fixed-point X toward negative infinity to an integer."
  (declare (ftype (function (fixnum) fixnum)) (pure t))
  (inline-letevals (x)
    (inline-quote (ash ,x (- eldoom-math-bits)))))

(define-inline eldoom-math-fixed-from-float (x)
  "Convert float X to fixed-point."
  (declare (ftype (function (number) fixnum)) (pure t))
  (inline-letevals (x)
    (inline-quote (floor (* ,x eldoom-math-unit-f)))))

(define-inline eldoom-math-fixed-to-float (x)
  "Convert fixed-point X to a float."
  (declare (ftype (function (fixnum) number)) (pure t))
  (inline-letevals (x)
    (inline-quote  (/ ,x eldoom-math-unit-f))))

(define-inline eldoom-math-fixed-ceil (x)
  "Round fixed-point X up to the next integer."
  (declare (ftype (function (fixnum) fixnum)) (pure t))
  (inline-letevals (x)
    (inline-quote (ash (+ ,x 65535) (- eldoom-math-bits)))))

;; bam = radians / (2 pi) * 2^32
(define-inline eldoom-math-radians-to-bam (radians)
  "Convert RADIANS to an unsigned 32-bit BAM angle."
  (declare (ftype (function (number) fixnum)) (pure t))
  (inline-letevals (radians)
    (inline-quote (eldoom-math-ang (round (* (/ ,radians (* 2.0 float-pi)) 4294967296.0))))))

;; bam = radians / 360 * 2^32
(define-inline eldoom-math-degrees-to-bam (degrees)
  "Convert RADIANS to an unsigned 32-bit BAM angle."
  (declare (ftype (function (number) fixnum)) (pure t))
  (inline-letevals (degrees)
    (inline-quote (eldoom-math-ang (round (* (/ ,degrees 360.0) 4294967296.0))))))

;; radians = bam * 2 pi / 2 ^ 32
(define-inline eldoom-math-bam-to-radians (bam)
  (declare (ftype (function (fixnum) number)) (pure t))
  (inline-letevals (bam)
    (inline-quote (/ (* ,bam 2.0 float-pi) 4294967296.0))))

(define-inline eldoom-math-bam-to-degrees (bam)
  (declare (ftype (function (fixnum) number)) (pure t))
  (inline-letevals (bam)
    (inline-quote (/ (* (eldoom-math-bam-to-radians ,bam) 180) float-pi))))

(define-inline eldoom-math-mul (a b)
  "Fixed-point multiply A by B (Doom FixedMul).
Operands are split into 16-bit halves to avoid bignum allocation on the
64-bit intermediate product."
  (declare (ftype (function (fixnum fixnum) fixnum)) (pure t))
  (inline-letevals (a b)
    (inline-quote
     (let ((a-h (ash ,a -16))
           (a-l (logand ,a 65535))
           (b-h (ash ,b -16))
           (b-l (logand ,b 65535)))
       (+ (ash (* a-h b-h) 16)
          (* a-h b-l)
          (* a-l b-h)
          (ash (* a-l b-l) -16))))))

(define-inline eldoom-math-div (a b)
  "Fixed-point divide A by B (Doom FixedDiv), saturating on overflow."
  (declare (ftype (function (fixnum fixnum) fixnum)) (pure t))
  (inline-letevals (a b)
    (inline-quote
     (if (>= (ash (abs ,a) -14) (abs ,b))
         (if (< (logxor ,a ,b) 0) eldoom-math-int-min eldoom-math-int-max)
       ;; (a << 16) / b, truncating toward zero like the C double cast.
       (let ((num (ash ,a 16)))
         (if (< (logxor num ,b) 0)
             (- (/ (- num) ,b))
           (/ num ,b)))))))

;; --- Trigonometric tables (generated at load, like Doom's tables.c) ---------

(defconst eldoom-math--finesine
  (let* ((len (+ eldoom-math-fine-angles (/ eldoom-math-fine-angles 4)))
         (table (make-vector len 0))
         (step (/ (* 2.0 float-pi) eldoom-math-fine-angles)))
    (dotimes (i len)
      (aset table i (round (* (sin (* (+ i 0.5) step)) eldoom-math-unit-f))))
    table)
  "finesine[i] = sin(i * 2pi/FINEANGLES) in 16.16; length FINEANGLES*5/4 so
`finecosine' can be read at an offset of FINEANGLES/4 without wrapping.")

(defconst eldoom-math--finecosine-offset (/ eldoom-math-fine-angles 4)
  "finecosine offset: finesine shifted a quarter turn (FINEANGLES/4).")

(defconst eldoom-math--finetangent
  (let* ((table (make-vector (/ eldoom-math-fine-angles 2) 0))
         (step (/ (* 2.0 float-pi) eldoom-math-fine-angles)))
    ;; finetangent[i] = tan((i+0.5)*step - PI/2) for i in 0..FINEANGLES/2-1.
    (dotimes (i (/ eldoom-math-fine-angles 2))
      (let ((a (- (* (+ i 0.5) step) (/ float-pi 2))))
        (aset table i (floor (* (tan a) eldoom-math-unit-f)))))
    table)
  "Tangent table covering (-90,90) degrees, indexed like Doom's finetangent.")

(defconst eldoom-math--tantoangle
  (let ((table (make-vector (1+ eldoom-math-slope-range) 0)))
    (dotimes (i (1+ eldoom-math-slope-range))
      (aset table i
            (eldoom-math-ang
             (round (* (/ (atan (/ (float i) eldoom-math-slope-range)) (* 2.0 float-pi))
                       4294967296.0)))))
    table)
  "tantoangle[i] = atan(i/SLOPERANGE) as a BAM angle, for i in 0..SLOPERANGE.")

;; (defconst eldoom-math-finetan-offset (eldoom-math-radians-to-bam (/ float-pi 2)))

(define-inline eldoom-math--fine-index (angle)
  "Fine-table index for BAM ANGLE."
  (declare (ftype (function (fixnum) fixnum)) (pure t))
  (inline-letevals (angle)
    (inline-quote
     (logand (ash (eldoom-math-ang ,angle) eldoom-math-angle-to-fine-shift) eldoom-math-fine-mask))))

(define-inline eldoom-math-sin (angle)
  "Sine of BAM ANGLE as a fixed-point value."
  (declare (ftype (function (fixnum) fixnum)) (pure t))
  (inline-letevals (angle)
    (inline-quote (aref eldoom-math--finesine (eldoom-math--fine-index ,angle)))))

(define-inline eldoom-math-cos (angle)
  "Cosine of BAM ANGLE as a fixed-point value."
  (declare (ftype (function (fixnum) fixnum)) (pure t))
  (inline-letevals (angle)
    (inline-quote
     (aref eldoom-math--finesine (+ (eldoom-math--fine-index ,angle) eldoom-math--finecosine-offset)))))

(define-inline eldoom-math--fine-tan-index (angle)
  (declare (ftype (function (fixnum) fixnum)) (pure t))
  (inline-letevals (angle)
    (inline-quote
     (logand (ash (eldoom-math-ang (+ ,angle eldoom-math-ang90)) eldoom-math-angle-to-fine-shift)
             eldoom-math-fine-tan-mask))))

(define-inline eldoom-math-tan (angle)
  (declare (ftype (function (fixnum) fixnum)) (pure t))
  (inline-letevals (angle)
    (inline-quote (aref eldoom-math--finetangent (eldoom-math--fine-tan-index ,angle)))))

(define-inline eldoom-math-fine-cot-index (angle)
  (declare (ftype (function (fixnum) fixnum)) (pure t))
  (inline-letevals (angle)
    (inline-quote
     (logand (ash (eldoom-math-ang ,angle) eldoom-math-angle-to-fine-shift) eldoom-math-fine-tan-mask))))

(defun eldoom-math-cot (angle)
  (declare (ftype (function (fixnum) fixnum)) (pure t))
  (- (aref eldoom-math--finetangent (eldoom-math-fine-cot-index angle))))

;; --- Angle/distance from coordinates (Doom r_main.c) ------------------------

(define-inline eldoom-math--atan (dy dx)
  "Calculates arctan of dy/dx, where dy >= 0, dx > 0 and dx >= dy"
  (declare (ftype (function (fixnum fixnum) fixnum)) (pure t))
  (inline-letevals (dy dx)
    (inline-quote (aref eldoom-math--tantoangle
                        ;; tantoangle array contains 2048 (2^11) values
                        ;; Note: this is more precise than the original SlopeDiv in the doom engine,
                        ;; which calculated ((dy << 3) / (dx >> 8)), to fit all the intermediate values
                        ;; within 32-bit wide integers.
                        (let ((ans (/ (ash ,dy 11) ,dx)))
                          (min ans eldoom-math-slope-range))))))

(defun eldoom-math-atan2 (dy dx)
  "BAM angle of the vector from (0,0) to (DX,DY)."
  (declare (ftype (function (fixnum fixnum) fixnum)) (pure t))
  (if (and (= dx 0) (= dy 0))
      0
    (if (>= dx 0)
        (if (>= dy 0)
            (if (> dx dy)
                (eldoom-math--atan dy dx)
              (eldoom-math-ang (- eldoom-math-ang90 1 (eldoom-math--atan dx dy))))
          (let ((dy (- dy)))
            (if (> dx dy)
                (eldoom-math-ang (- (eldoom-math--atan dy dx)))
              (eldoom-math-ang (+ eldoom-math-ang270 (eldoom-math--atan dx dy))))))
      (let ((dx (- dx)))
        (if (>= dy 0)
            (if (> dx dy)
                (eldoom-math-ang (- eldoom-math-ang180 1 (eldoom-math--atan dy dx)))
              (eldoom-math-ang (+ eldoom-math-ang90 (eldoom-math--atan dx dy))))
          (let ((dy (- dy)))
            (if (> dx dy)
                (eldoom-math-ang (+ eldoom-math-ang180 (eldoom-math--atan dy dx)))
              (eldoom-math-ang (- eldoom-math-ang270 1 (eldoom-math--atan dx dy))))))))))

(defun eldoom-math-hypot (dx dy)
  "Calculates value of sqrt(DX*DX+DY*DY) using fixed point trigonometry
instead of a square root."
  (declare (ftype (function (fixnum fixnum) fixnum)) (pure t))
  (setf dx (abs dx)
        dy (abs dy))
  (when (> dy dx)
    (cl-rotatef dx dy))
  (if (= dx 0)
      0
    (let* ((angle (eldoom-math--atan dy dx)))
      (eldoom-math-div dx (eldoom-math-cos angle)))))

(defun eldoom-math-intercept-frac (x1 y1 dx1 dy1 x2 y2 dx2 dy2)
  "Fixed_t fraction along ray (X1,Y1)+T*(DX1,DY1) to line (X2,Y2)+S*(DX2,DY2)"
  (declare (ftype (function (fixnum fixnum fixnum fixnum fixnum fixnum fixnum fixnum) fixnum)) (pure t))
  (let ((den (- (eldoom-math-mul dx1 dy2) (eldoom-math-mul dy1 dx2))))
    (if (= 0 den)
        0
      (let ((num (- (eldoom-math-mul (- x2 x1) dy2) (eldoom-math-mul (- y2 y1) dx2))))
        (eldoom-math-div num den)))))

(defun eldoom-math-has-flag (flags flag)
  (declare (ftype (function (fixnum fixnum) boolean)) (pure t) (pure t))
  (/= 0 (logand flags flag)))

(define-inline eldoom-math-clear-flag (flags flag)
  (declare (ftype (function (fixnum fixnum) fixnum)) (pure t))
  (inline-letevals (flags flag)
    (inline-quote (logand ,flags (lognot ,flag)))))

(defmacro eldoom-math-clear-flagf (flags flag)
  (gv-letplace (getter setter) flags
    (funcall setter `(logand ,getter (lognot ,flag)))))

(defmacro eldoom-math-set-flagf (flags flag)
  (gv-letplace (getter setter) flags
    (funcall setter `(logior ,getter ,flag))))

(define-inline eldoom-math-clamp (value min-value max-value)
  (declare (ftype (function (fixnum fixnum fixnum) fixnum)) (pure t))
  (inline-letevals (value min-value max-value)
    (inline-quote (max ,min-value (min ,value ,max-value)))))

(cl-defstruct (eldoom-math-segment
               (:constructor eldoom-math-make-segment)
               (:copier nil))
  (x1 0 :type fixnum) (y1 0 :type fixnum)
  (x2 0 :type fixnum) (y2 0 :type fixnum))

(define-inline eldoom-math-cross (ax-fx ay-fx bx-fx by-fx)
  (declare (ftype (function (fixnum fixnum fixnum fixnum) fixnum)) (pure t))
  (inline-letevals (ax-fx ay-fx bx-fx by-fx)
    (inline-quote (- (eldoom-math-mul ,ax-fx ,by-fx) (eldoom-math-mul ,ay-fx ,bx-fx)))))

(defun eldoom-math-point-segment-orientation (x y seg)
  (declare (ftype (function (fixnum fixnum eldoom-math-segment) symbol)) (pure t))
  (let ((x1 (eldoom-math-segment-x1 seg))
        (y1 (eldoom-math-segment-y1 seg))
        (x2 (eldoom-math-segment-x2 seg))
        (y2 (eldoom-math-segment-y2 seg)))
    (cond
     ((= x1 x2)
      (cond
       ((= x x1) :collinear)
       ((< x x1) (if (< y2 y1) :right :left))
       (t (if (> y2 y1) :right :left))))
     ((= y1 y2)
      (cond
       ((= y y1) :collinear)
       ((< y y1) (if (> x2 x1) :right :left))
       (t (if (< x2 x1) :right :left))))
     (t
      (let ((cross (eldoom-math-cross (- x2 x1) (- y2 y1) (- x x1) (- y y1))))
        (cond
         ((< cross 0) :right)
         ((> cross 0) :left)
         (t :collinear)))))))


(defun eldoom-math-on-right-side-p (x y seg)
  (declare (ftype (function (fixnum fixnum eldoom-math-segment) boolean)) (pure t))
  (eql (eldoom-math-point-segment-orientation x y seg) :right))

(defun eldoom-math-segment-angle (seg)
  (declare (ftype (function (eldoom-math-segment) fixnum)) (pure t))
  (let ((dx (- (eldoom-math-segment-x2 seg) (eldoom-math-segment-x1 seg)))
        (dy (- (eldoom-math-segment-y2 seg) (eldoom-math-segment-y1 seg))))
    (eldoom-math-atan2 dy dx)))

(defun eldoom-math-ray-intersection-fraction (ray1 ray2)
  "Fraction along ray1 to ray2
P_InterceptVector2 in p_sight.c and P_InterceptVector in p_maputl.c"
  (declare (ftype (function (eldoom-math-segment eldoom-math-segment) fixnum)) (pure t))
  (let* ((ray1-x1 (eldoom-math-segment-x1 ray1))
         (ray1-y1 (eldoom-math-segment-y1 ray1))
         (ray1-x2 (eldoom-math-segment-x2 ray1))
         (ray1-y2 (eldoom-math-segment-y2 ray1))
         (ray1-dx (- ray1-x2 ray1-x1))
         (ray1-dy (- ray1-y2 ray1-y1))
         (ray2-x1 (eldoom-math-segment-x1 ray2))
         (ray2-y1 (eldoom-math-segment-y1 ray2))
         (ray2-x2 (eldoom-math-segment-x2 ray2))
         (ray2-y2 (eldoom-math-segment-y2 ray2))
         (ray2-dx (- ray2-x2 ray2-x1))
         (ray2-dy (- ray2-y2 ray2-y1))
         (den (eldoom-math-cross ray1-dx ray1-dy ray2-dx ray2-dy)))
    (if (= 0 den)
        0
      (let ((num (eldoom-math-cross (- ray2-x1 ray1-x1) (- ray2-y1 ray1-y1) ray2-dx ray2-dy)))
        (eldoom-math-div num den)))))

(cl-defstruct (eldoom-math-bbox
               (:constructor eldoom-math-make-bbox)
               (:copier nil))
  (left 0 :type fixnum)
  (right 0 :type fixnum)
  (top 0 :type fixnum)
  (bottom 0 :type fixnum))

(defun eldoom-math-line-intersects-bbox-p (line bbox)
  (declare (ftype (function (eldoom-math-segment eldoom-math-bbox) boolean)) (pure t))
  (let* ((x1 (eldoom-math-segment-x1 line))
         (y1 (eldoom-math-segment-y1 line))
         (dx (- (eldoom-math-segment-x2 line) x1))
         (dy (- (eldoom-math-segment-y2 line) y1)))
    (cond
     ((zerop dy) ;; horizontal
      (not (eql (> (eldoom-math-bbox-top bbox) y1)
                (> (eldoom-math-bbox-bottom bbox) y1))))
     ((zerop dx) ;; vertical
      (not (eql (< (eldoom-math-bbox-right bbox) x1)
                (< (eldoom-math-bbox-left bbox) x1))))
     ((> (eldoom-math-div dy dx) 0) ;; positive slope
      (not (eql (eldoom-math-on-right-side-p (eldoom-math-bbox-left bbox)
                                             (eldoom-math-bbox-top bbox) line)
                (eldoom-math-on-right-side-p (eldoom-math-bbox-right bbox)
                                             (eldoom-math-bbox-bottom bbox) line))))
     (t ;; negative slope
      (not (eql (eldoom-math-on-right-side-p (eldoom-math-bbox-right bbox)
                                             (eldoom-math-bbox-top bbox) line)
                (eldoom-math-on-right-side-p (eldoom-math-bbox-left bbox)
                                             (eldoom-math-bbox-bottom bbox) line)))))))

(defun eldoom-math-bboxes-intersect-p (bbox1 bbox2)
  "Return t iff BBOX1 and BBOX2 have non-empty intersection."
  (declare (ftype (function (eldoom-math-bbox eldoom-math-bbox) boolean)) (pure t))
  (and (> (eldoom-math-bbox-right bbox1) (eldoom-math-bbox-left bbox2))
       (< (eldoom-math-bbox-left bbox1) (eldoom-math-bbox-right bbox2))
       (> (eldoom-math-bbox-top bbox1) (eldoom-math-bbox-bottom bbox2))
       (< (eldoom-math-bbox-bottom bbox1) (eldoom-math-bbox-top bbox2))))

(cl-defstruct (eldoom-math-point-2d
               (:constructor eldoom-math-make-point-2d)
               (:copier nil))
  x y)

(cl-defstruct (eldoom-math-point-3d
               (:constructor eldoom-math-make-point-3d)
               (:copier nil))
  x y z)

(cl-defstruct (eldoom-math-view-transformation
               (:constructor eldoom-math-make-view-transformation)
               (:copier nil))
  x y z ;; player position
  sin cos ;; sine and cosine of the viewing angle
  )

(defun eldoom-math-set-view-transformation (transformation x y z angle)
  (setf (eldoom-math-view-transformation-x transformation) x
        (eldoom-math-view-transformation-y transformation) y
        (eldoom-math-view-transformation-z transformation) z
        (eldoom-math-view-transformation-sin transformation) (eldoom-math-sin angle)
        (eldoom-math-view-transformation-cos transformation) (eldoom-math-cos angle)))

(define-inline eldoom-math-apply-view-transformation-x (transformation x y)
  (declare (ftype (function (eldoom-math-view-transformation fixed fixed) fixed)) (pure t))
  (inline-letevals (transformation x y)
    (inline-quote
     (+ (eldoom-math-mul (eldoom-math-view-transformation-cos ,transformation) 
                         (- ,x (eldoom-math-view-transformation-x ,transformation)))
        (eldoom-math-mul (eldoom-math-view-transformation-sin ,transformation) 
                         (- ,y (eldoom-math-view-transformation-y ,transformation)))))))

(define-inline eldoom-math-apply-view-transformation-y (transformation x y)
  (declare (ftype (function (eldoom-math-view-transformation fixed fixed) fixed)) (pure t))
  (inline-letevals (transformation x y)
    (inline-quote
     (- (eldoom-math-mul (eldoom-math-view-transformation-cos ,transformation) 
                         (- ,y (eldoom-math-view-transformation-y ,transformation)))
        (eldoom-math-mul (eldoom-math-view-transformation-sin ,transformation)
                         (- ,x (eldoom-math-view-transformation-x ,transformation)))))))

(define-inline eldoom-math-apply-view-transformation-z (transformation z)
  (declare (ftype (function (eldoom-math-view-transformation fixed) fixed)) (pure t))
  (inline-letevals (transformation z)
    (inline-quote (- ,z (eldoom-math-view-transformation-z ,transformation)))))

(cl-defstruct (eldoom-math-projection
               (:constructor eldoom-math-make-projection)
               (:copier nil))
  half-width half-height ;; viewport dimensions
  fov-scale)

(defun eldoom-math-set-projection (projection width height fov)
  (let* ((half-width (/ width 2))
         (half-height (/ height 2))
         (half-fov (/ fov 2))
         (tan-half-fov (eldoom-math-tan half-fov)))
    (setf (eldoom-math-projection-half-width projection) half-width
          (eldoom-math-projection-half-height projection) half-height
          (eldoom-math-projection-fov-scale projection) (eldoom-math-div half-width tan-half-fov))))

(define-inline eldoom-math-apply-projection-x (projection x y)
  (declare (ftype (function (eldoom-math-view-projection fixed fixed) fixed)) (pure t))
  (inline-letevals (projection x y)
    (inline-quote
     (- (eldoom-math-projection-half-width ,projection)
        (eldoom-math-div (eldoom-math-mul ,x (eldoom-math-projection-fov-scale ,projection)) ,y)))))

(define-inline eldoom-math-apply-projection-z (projection z y)
  (declare (ftype (function (eldoom-math-view-projection fixed fixed) fixed)) (pure t))
  (inline-letevals (projection z y)
    (inline-quote
     (- (eldoom-math-projection-half-height ,projection) 
        (eldoom-math-div (eldoom-math-mul ,z (eldoom-math-projection-fov-scale ,projection)) y)))))

(provide 'eldoom-math)

;; End:
;;; eldoom-math.el ends here
