;;; eldoom-sight.el --- Doom visibility -*- lexical-binding:t -*-

;; Copyright (C) 2026 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; Checking sight from point to point

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'eldoom-globals)
(require 'eldoom-info)
(require 'eldoom-math)
(require 'eldoom-mobj)
(require 'eldoom-wad)

(defun eldoom-sight--segments-intersect (seg1 seg2)
  (declare (ftype (function (eldoom-math-segment eldoom-math-segment) boolean)) (pure t))
  (let ((s1-x1 (eldoom-math-segment-x1 seg1))
        (s1-y1 (eldoom-math-segment-y1 seg1))
        (s1-x2 (eldoom-math-segment-x2 seg1))
        (s1-y2 (eldoom-math-segment-y2 seg1))
        (s2-x1 (eldoom-math-segment-x1 seg2))
        (s2-y1 (eldoom-math-segment-y1 seg2))
        (s2-x2 (eldoom-math-segment-x2 seg2))
        (s2-y2 (eldoom-math-segment-y2 seg2)))
    (and (not (eql (eldoom-math-point-segment-orientation s1-x1 s1-y1 seg2)
                   (eldoom-math-point-segment-orientation s1-x2 s1-y2 seg2)))
         (not (eql (eldoom-math-point-segment-orientation s2-x1 s2-y1 seg1)
                   (eldoom-math-point-segment-orientation s2-x2 s2-y2 seg1))))))

(cl-defstruct (eldoom-sight--slope-range (:constructor eldoom-sight--make-slope-range)
                                         (:copier nil))
  (top-slope 0 :type fixnum)
  (bottom-slope 0 :type fixnum))

(defun eldoom-sight--cross-line (linedef segment z-start slope-range)
  "Throws :blocks t if linedef blocks view from Z-START with given SLOPE-RANGE.
Otherwise updates the SLOPE-RANGE.
Part of P_CrossSubsector in p_sight.c"
  (let ((line-seg (ewd-linedef-segment linedef)))
    (when (eldoom-sight--segments-intersect segment line-seg)
      (when (not (eldoom-math-has-flag (ewd-linedef-flags linedef) enf-ML_TWOSIDED))
        (throw :blocks t))
      (let* ((front (ewd-linedef-front-sector linedef))
             (front-floor (ewd-sector-floor-height-fx front))
             (front-ceiling (ewd-sector-ceiling-height-fx front))
             (back (ewd-linedef-back-sector linedef))
             (back-floor (ewd-sector-floor-height-fx back))
             (back-ceiling (ewd-sector-ceiling-height-fx back)))
        (when (or (/= front-floor back-floor) (/= front-ceiling back-ceiling))
          (let ((open-top (min front-ceiling back-ceiling))
                (open-bottom (max front-floor back-floor)))
            (when (>= open-bottom open-top)
              (throw :blocks t))
            (let ((frac (eldoom-math-ray-intersection-fraction segment line-seg))
                  (slope-bottom (eldoom-sight--slope-range-bottom-slope slope-range))
                  (slope-top (eldoom-sight--slope-range-top-slope slope-range)))
              (when (/= front-floor back-floor)
                (cl-callf max slope-bottom (eldoom-math-div (- open-bottom z-start)
                                                            frac)))
              (when (/= front-ceiling back-ceiling)
                (cl-callf min slope-top (eldoom-math-div (- open-top z-start) frac)))
              (when (<= slope-top slope-bottom)
                (throw :blocks t))
              (setf (eldoom-sight--slope-range-bottom-slope slope-range) slope-bottom
                    (eldoom-sight--slope-range-top-slope slope-range) slope-top))))))))

(defun eldoom-sight--cross-subsector (subsector segment z-start slope-range)
  "Returns not nil if the trace crosses the subsector successfully
P_CrossSubsector in p_sight.c"
  (let* ((segs (ewd-subsector-segs subsector)))
    (not 
     (catch :blocks
       (cl-loop for seg across segs do
                (let ((linedef (ewd-seg-linedef seg)))
                  (when (/= (ewd-linedef-iteration linedef) eldoom-globals-iteration)
                    (setf (ewd-linedef-iteration linedef) eldoom-globals-iteration)
                    (eldoom-sight--cross-line linedef segment z-start slope-range))))
       nil))))

(defun eldoom-sight--cross-bsp-node (node segment z-start slope-range)
  "P_CrossBSPNode in p_sight.c"
  (if (ewd-subsector-p node)
      (eldoom-sight--cross-subsector node segment z-start slope-range)
    (let* ((x1 (eldoom-math-segment-x1 segment))
           (y1 (eldoom-math-segment-y1 segment))
           (x2 (eldoom-math-segment-x2 segment))
           (y2 (eldoom-math-segment-y2 segment))
           (side (eldoom-math-point-segment-orientation x1 y1 (ewd-node-segment node)))
           (start-child (cl-ecase side
                          ((:right :collinear) (ewd-node-right-child node))
                          (:left (ewd-node-left-child node))))
           (other-child (cl-ecase side
                          ((:right :collinear) (ewd-node-left-child node))
                          (:left (ewd-node-right-child node)))))
      (when (eldoom-sight--cross-bsp-node start-child segment z-start slope-range)
        (or (eql side (eldoom-math-point-segment-orientation x2 y2 (ewd-node-segment node)))
            (eldoom-sight--cross-bsp-node other-child segment z-start slope-range))))))

(defvar-local eldoom-sight--segment nil)
(defvar-local eldoom-sight--slope-range nil)

(defun eldoom-sight-check (from to)
  "Return non-nil if mobj FROM has unobstructed horizontal LOS to mobj TO
 (lines only).
P_CheckSight in p_sight.c"
  (let ((from-sector (ewd-subsector-sector (emo-subsector from)))
        (to-sector (ewd-subsector-sector (emo-subsector to))))
    (when (not (aref (ewd-sector-reject from-sector) (ewd-sector-index to-sector)))
      (when (not eldoom-sight--segment)
        (setq eldoom-sight--segment (eldoom-math-make-segment)))
      (when (not eldoom-sight--slope-range)
        (setq eldoom-sight--slope-range (eldoom-sight--make-slope-range)))
      (cl-incf eldoom-globals-iteration) 
      (let ((z-start (- (+ (emo-z-fx from) (emo-height-fx from))
                        (/ (emo-height-fx from) 4))))
        (setf (eldoom-math-segment-x1 eldoom-sight--segment) (emo-x-fx from)
              (eldoom-math-segment-y1 eldoom-sight--segment) (emo-y-fx from)
              (eldoom-math-segment-x2 eldoom-sight--segment) (emo-x-fx to)
              (eldoom-math-segment-y2 eldoom-sight--segment) (emo-y-fx to)
              (eldoom-sight--slope-range-top-slope eldoom-sight--slope-range)
              (- (+ (emo-z-fx to) (emo-height-fx to)) z-start)
              (eldoom-sight--slope-range-bottom-slope eldoom-sight--slope-range)
              (- (emo-z-fx to) z-start))
        (eldoom-sight--cross-bsp-node (ewd-map-root-node ewd-current-map)
                                      eldoom-sight--segment
                                      z-start eldoom-sight--slope-range)))))


;; Local Variables:
;; read-symbol-shorthands: (("emo-" . "eldoom-mobj-") ("ewd-" . "eldoom-wad-") ("enf-" . "eldoom-info-"))
;; End:
(provide 'eldoom-sight)
;;; eldoom-sight.el ends here
