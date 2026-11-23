;;; eldoom-map.el --- Doom map queries -*- lexical-binding:t -*-

;; Copyright (C) 2026 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'eldoom-globals)
(require 'eldoom-info)
(require 'eldoom-math)
(require 'eldoom-wad)

(defconst emp-block-units 128)
(defconst emp-block-size (* emp-block-units eldoom-math-unit))

(defsubst emp-point-in-right-child-p (node x-fx y-fx)
  "Returns true iff point is on the right side of node's partition line"
  (eql (eldoom-math-point-segment-orientation x-fx y-fx (ewd-node-segment node))
       :right))

(defun emp--subsector-at (node x y)
  "Find the subsector in NODE's tree containing X,Y."
  (while (ewd-node-p node)
    (setq node (if (emp-point-in-right-child-p node x y)
                   (ewd-node-right-child node)
                 (ewd-node-left-child node))))
  node)

(defun emp-subsector-at (x y)
  "Find the subsector containing map containing X,Y."
  (emp--subsector-at (ewd-map-root-node ewd-current-map) x y))

(defun emp-sector-at (x y)
  "Find the sector containing map coordinates X,Y."
  (ewd-subsector-sector (emp-subsector-at x y)))

(defun emp-blockmap-index (blockmap bx by)
  (+ bx (* by (ewd-blockmap-column-count blockmap))))

(defun emp-for-each-line-in-block (blockmap bx by fn)
  "Call FN for each linedef in given blockmap block.
P_BlockLinesIterator in p_maputl.c"
  (when (and (<= 0 bx)
             (< bx (ewd-blockmap-column-count blockmap))
             (<= 0 by)
             (< by (ewd-blockmap-row-count blockmap)))
    (let* ((blockmap-index (emp-blockmap-index blockmap bx by))
           (lines-blockmap (ewd-blockmap-lines-blockmap blockmap))
           (linedefs (aref lines-blockmap blockmap-index)))
      (cl-loop for linedef across linedefs do
               (when (/= eldoom-globals-iteration (ewd-linedef-iteration linedef))
                 (setf (ewd-linedef-iteration linedef) eldoom-globals-iteration)
                 (funcall fn linedef))))))

(defun emp-for-each-line-in-block-overlapping-bbox (left right bottom top fn)
  (let* ((blockmap (ewd-map-blockmap ewd-current-map))
         (origin-x (ewd-blockmap-origin-x-fx blockmap))
         (origin-y (ewd-blockmap-origin-y-fx blockmap))
         (bl (ash (- left origin-x) ewd-map-block-shift))
         (br (ash (- right origin-x) ewd-map-block-shift))
         (bb (ash (- bottom origin-y) ewd-map-block-shift))
         (bt (ash (- top origin-y) ewd-map-block-shift)))
    (cl-loop for bx from bl to br do
             (cl-loop for by from bb to bt do
                      (emp-for-each-line-in-block blockmap bx by fn)))))

(defun emp-approx-distance (x1 y1 x2 y2)
  "Fast estimation of distance between two points"
  (let* ((dx (abs (- x1 x2)))
         (dy (abs (- y1 y2)))
         (sum (+ dx dy)))
    (if (< dx dy)
        (- sum (ash dx -1))
      (- sum (ash dy -1)))))

(defun emp-next-sector (linedef sector)
  "getNextSector in p_spec.h"
  (let ((flags (ewd-linedef-flags linedef)))
    (when (/= 0 (logand flags enf-ML_TWOSIDED))
      (if (eq sector (ewd-linedef-front-sector linedef))
          (ewd-linedef-back-sector linedef)
        (ewd-linedef-front-sector linedef)))))

(defun emp-lowest-floor-surrounding-sector (sector)
  "P_FindLowestFloorSurrounding in p_spec.c"
  (let ((height (ewd-sector-floor-height-fx sector)))
    (cl-loop for linedef across (ewd-sector-linedefs sector) do
             (when-let* ((other (emp-next-sector linedef sector)))
               (setq height (min (ewd-sector-floor-height-fx other) height))))
    height))

(defun emp-highest-floor-surrounding-sector (sector)
  "P_FindHighestFloorSurrounding in p_spec.c"
  (let ((height (eldoom-math-fixed-from-int -500)))
    (cl-loop for linedef across (ewd-sector-linedefs sector) do
             (when-let* ((other (emp-next-sector linedef sector)))
               (setq height (max (ewd-sector-floor-height-fx other) height))))
    height))

(defun emp-lowest-ceiling-surrounding-sector (sector)
  "P_FindLowestCeilingSurrounding in p_spec.c"
  (let ((height eldoom-math-int-max))
    (cl-loop for linedef across (ewd-sector-linedefs sector) do
             (when-let* ((other (emp-next-sector linedef sector)))
               (setq height (min (ewd-sector-ceiling-height-fx other) height))))
    height))

(defun emp-highest-ceiling-surrounding-sector (sector)
  "P_FindHighestCeilingSurrounding in p_spec.c"
  (let ((height 0))
    (cl-loop for linedef across (ewd-sector-linedefs sector) do
             (when-let* ((other (emp-next-sector linedef sector)))
               (setq height (max (ewd-sector-ceiling-height-fx other) height))))
    height))

(defun emp-next-highest-floor (sector current-height)
  "P_FindNextHeighestFloor in p_spec.c"
  (let ((height eldoom-math-int-max))
    (cl-loop for linedef across (ewd-sector-linedefs sector) do
             (when-let* ((other (emp-next-sector linedef sector)))
               (let ((floor-height (ewd-sector-floor-height-fx other)))
                 (when (< current-height floor-height height)
                   (setq height floor-height)))))
    (if (/= height eldoom-math-int-max) height current-height)))

(defun emp-min-surrounding-light (sector current-light)
  "P_FindMinSurroundingLight in p_spec.c"
  (let ((min-light current-light))
    (cl-loop for linedef across (ewd-sector-linedefs sector) do
             (when-let* ((other (emp-next-sector linedef sector)))
               (setq min-light (min min-light (ewd-sector-light other)))))
    min-light))

;; Local Variables:
;; read-symbol-shorthands: (("ewd-" . "eldoom-wad-") ("emp-" . "eldoom-map-") ("enf-" . "eldoom-info-"))
;; End:
(provide 'eldoom-map)
;;; eldoom-map.el ends here
