;;; eldoom-math-test.el --- Tests for fixed-point and BAM trigonometry -*- lexical-binding:t -*-

;; Copyright (C) 2026 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'eldoom-math)


(defun eldoom-math-test--close-p (f1 f2)
  (< (abs (- f1 f2)) 0.001))

;; Precision of the atan2 function in radians (0.026 degrees)
(defun eldoom-math-test--angle-close-p (a1 a2)
  (< (abs (- a1 a2)) 0.000454))

;; If the range does not wrap around the inf (that is f1 <= f2),
;; the expected value range is [f1, f2].
;; Otherwise it's [-inf, f1] + [f2, inf]
(defun eldoom-math-test--between-p (v f1 f2)
  (if (<= f1 f2)
      (and (<= f1 v)
           (<= v f2))
    (or (<= v f2)
        (<= f1 v))))

(defun eldoom-math-test--tan-matches (angle-rad)
  (let*  ((angle-bam (eldoom-math-radians-to-bam angle-rad))
          (value (eldoom-math-fixed-to-float (eldoom-math-tan angle-bam)))
          (angle-step (/ (* 2.0 float-pi) eldoom-math-fine-angles))
          (next-value (tan (+ angle-rad angle-step)))
          (prev-value (tan (- angle-rad angle-step))))
    (eldoom-math-test--between-p value prev-value next-value)))
(defun eldoom-math-test--tan-matches-explainer (angle-rad)
  (let* ((angle-bam (eldoom-math-radians-to-bam angle-rad))
         (value (eldoom-math-tan angle-bam))
         (angle-step (/ (* 2.0 float-pi) eldoom-math-fine-angles))
         (next-value (tan (+ angle-rad angle-step)))
         (prev-value (tan (- angle-rad angle-step))))
    (format "tan(%f) = %f (fixed: %d), expected in range [%f,%f]"
            angle-rad
            (eldoom-math-fixed-to-float value) value
            prev-value next-value)))
(put 'eldoom-math-test--tan-matches 'ert-explainer #'eldoom-math-test--tan-matches-explainer)


(defsubst eldoom-math--cot-test-impl (angle) (/ 1.0 (tan angle)))

(defun eldoom-math-test--cot-matches (angle-rad)
  (let*  ((angle-bam (eldoom-math-radians-to-bam angle-rad))
          (value (eldoom-math-fixed-to-float (eldoom-math-cot angle-bam)))
          (angle-step (/ (* 2.0 float-pi) eldoom-math-fine-angles))
          (next-value (eldoom-math--cot-test-impl (+ angle-rad angle-step)))
          (prev-value (eldoom-math--cot-test-impl (- angle-rad angle-step))))
    (eldoom-math-test--between-p value next-value prev-value)))
(defun eldoom-math-test--cot-matches-explainer (angle-rad)
  (let* ((angle-bam (eldoom-math-radians-to-bam angle-rad))
         (value (eldoom-math-cot angle-bam))
         (angle-step (/ (* 2.0 float-pi) eldoom-math-fine-angles))
         (next-value (eldoom-math--cot-test-impl (+ angle-rad angle-step)))
         (prev-value (eldoom-math--cot-test-impl (- angle-rad angle-step))))
    (format "cot(%f) = %f (fixed: %d), expected in range [%f,%f]"
            angle-rad
            (eldoom-math-fixed-to-float value) value
            next-value prev-value)))
(put 'eldoom-math-test--cot-matches 'ert-explainer #'eldoom-math-test--cot-matches-explainer)


(defun eldoom-math-test--sin-matches (angle-rad)
  (let*  ((angle-bam (eldoom-math-radians-to-bam angle-rad))
          (value (eldoom-math-fixed-to-float (eldoom-math-sin angle-bam)))
          (expected-value (sin angle-rad)))
    (eldoom-math-test--close-p value expected-value)))
(defun eldoom-math-test--sin-matches-explainer (angle-rad)
  (let* ((angle-bam (eldoom-math-radians-to-bam angle-rad))
         (value-fx (eldoom-math-sin angle-bam))
         (value (eldoom-math-fixed-to-float value-fx))
         (expected-value (sin angle-rad)))
    (format "sin(%f (bam: %d)) = %f (fixed: %d), expected %f (fixed: %d), difference: %f"
            angle-rad angle-bam 
            value value-fx
            expected-value (eldoom-math-fixed-from-float expected-value)
            (abs (- value expected-value)))))
(put 'eldoom-math-test--sin-matches 'ert-explainer #'eldoom-math-test--sin-matches-explainer)

(defun eldoom-math-test--cos-matches (angle-rad)
  (let*  ((angle-bam (eldoom-math-radians-to-bam angle-rad))
          (value (eldoom-math-fixed-to-float (eldoom-math-cos angle-bam)))
          (expected-value (cos angle-rad)))
    (eldoom-math-test--close-p value expected-value)))
(defun eldoom-math-test--cos-matches-explainer (angle-rad)
  (let* ((angle-bam (eldoom-math-radians-to-bam angle-rad))
         (value-fx (eldoom-math-cos angle-bam))
         (value (eldoom-math-fixed-to-float value-fx))
         (expected-value (cos angle-rad)))
    (format "cos(%f (bam: %d)) = %f (fixed: %d), expected %f (fixed: %d), difference: %f"
            angle-rad angle-bam 
            value value-fx
            expected-value (eldoom-math-fixed-from-float expected-value)
            (abs (- value expected-value)))))
(put 'eldoom-math-test--cos-matches 'ert-explainer #'eldoom-math-test--cos-matches-explainer)


(defun eldoom-math-test--normalize-angle (angle)
  "Returns ANGLE normalized to range (-pi, pi]"
  (while (> angle float-pi)
    (setq angle (- angle (* 2.0 float-pi))))
  (while (<= angle (- float-pi))
    (setq angle (+ angle (* 2.0 float-pi))))
  angle)

(defun eldoom-math-test--atan2-matches (dy dx)
  (let* ((value (eldoom-math-test--normalize-angle (eldoom-math-bam-to-radians (eldoom-math-atan2 dy dx))))
         (expected-value (atan dy dx)))
    (eldoom-math-test--angle-close-p value expected-value)))
(defun eldoom-math-test--atan2-matches-explainer (dy dx)
  (let* ((value-bam (eldoom-math-atan2 dy dx))
         (value (eldoom-math-test--normalize-angle (eldoom-math-bam-to-radians value-bam)))
         (expected-value (atan dy dx)))
    (format "atan(%d,%d) = %f (BAM: %d), expected %f (BAM: %d), difference: %f"
            dy dx
            value value-bam
            expected-value (eldoom-math-radians-to-bam expected-value)
            (abs (- value expected-value)))))
(put 'eldoom-math-test--atan2-matches 'ert-explainer #'eldoom-math-test--atan2-matches-explainer)


(ert-deftest sin-values ()
  (cl-loop for i from -720 to 720 do
           (let* ((angle-rad (/ (* i float-pi) 180)))
             (should (eldoom-math-test--sin-matches angle-rad)))))

(ert-deftest cos-values ()
  (cl-loop for i from -720 to 720 do
           (let* ((angle-rad (/ (* i float-pi) 180)))
             (should (eldoom-math-test--cos-matches angle-rad)))))

(ert-deftest tan-values ()
  (cl-loop for i from -360 to 360 do
           (let* ((angle-rad (/ (* i float-pi) 180)))
             (should (eldoom-math-test--tan-matches angle-rad)))))

(ert-deftest cot-values ()
  (cl-loop for i from -360 to 360 do
           (let* ((angle-rad (/ (* i float-pi) 180)))
             (should (eldoom-math-test--cot-matches angle-rad)))))

(ert-deftest atan2 ()
  (cl-loop for dx from -32000 to 32000 by 1000 do
           (cl-loop for dy from -32000 to 32000 by 1000 do
                    (should (eldoom-math-test--atan2-matches dy dx)))))

(ert-deftest mul ()
  (cl-loop for a from -128.5 to 128.5 by 16 do
           (cl-loop for b from -128.5 to 128.5 by 16 do
                    (let ((product (eldoom-math-mul (eldoom-math-fixed-from-float a) (eldoom-math-fixed-from-float b)))
                          (expected-product (eldoom-math-fixed-from-float (* a b))))
                      (should (= product expected-product))))))

(ert-deftest ray-intersection-fraction ()
  (let* ((ray1 (eldoom-math-make-segment :x1 -256 :y1 0 :x2 256 :y2 0))
         (ray2 (eldoom-math-make-segment :x1 0 :y1 256 :x2 0 :y2 -256))
         (frac (eldoom-math-ray-intersection-fraction ray1 ray2)))
    (should (= frac (eldoom-math-fixed-from-float 0.5))))
  (let* ((ray1 (eldoom-math-make-segment :x1 256 :y1 0 :x2 -256 :y2 0))
         (ray2 (eldoom-math-make-segment :x1 0 :y1 256 :x2 0 :y2 -256))
         (frac (eldoom-math-ray-intersection-fraction ray1 ray2)))
    (should (= frac (eldoom-math-fixed-from-float 0.5))))
  (let* ((ray1 (eldoom-math-make-segment :x1 -256 :y1 -256 :x2 768 :y2 768))
         (ray2 (eldoom-math-make-segment :x1 0 :y1 256 :x2 0 :y2 -256))
         (frac (eldoom-math-ray-intersection-fraction ray1 ray2)))
    (should (= frac (eldoom-math-fixed-from-float 0.25))))

)

(provide 'eldoom-math-test)

;; End:
;;; eldoom-math-test.el ends here
