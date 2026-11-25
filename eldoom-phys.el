;;; eldoom-phys.el --- Doom movement and collision -*- lexical-binding:t -*-

;; Copyright (C) 2026 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; Player movement and wall collision modelled on vanilla Doom (p_map.c,
;; p_maputl.c): blockmap line iteration, PIT_CheckLine, P_CheckPosition,
;; P_TryMove, P_SlideMove, P_XYMovement subdivision, and PIT_CheckThing.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'eldoom-globals)
(require 'eldoom-info)
(require 'eldoom-map)
(require 'eldoom-math)
(require 'eldoom-mobj)
(require 'eldoom-random)
(require 'eldoom-trace)
(require 'eldoom-wad)

(defconst eph--mapblockshift 7)

(defconst eph--ml-blocking 1)
(defconst eph--ml-blockmonsters 2)

(defconst eph--max-step-height-fx (eldoom-math-fixed-from-int 24))

(defvar-local eph--tmfloorz-fx 0)
(defvar-local eph--tmceilingz-fx 0)
(defvar-local eph--tmdropoffz-fx 0)
(defvar-local eph--tmbbox nil)
(defvar-local eph--tmthing nil)
(defvar-local eph--tmx-fx 0)
(defvar-local eph--tmy-fx 0)
(defvar-local eph--tmx-move 0)
(defvar-local eph--tmy-move 0)
(defvar-local eph-linedef-hit nil "Head of the list of special linedefs hit
by the most recent call to eph-check-position. List is connected by next-hit
field.")

(defun eph--check-line (linedef)
  "Throw :hit with non-nil value if LINEDEF blocks the current test move.
PIT_CheckLine in p_map.c"
  (cond
   ((or (not (eldoom-math-bboxes-intersect-p eph--tmbbox (ewd-linedef-bbox linedef)))
        (not (eldoom-math-line-intersects-bbox-p (ewd-linedef-segment linedef) eph--tmbbox)))
    t)
   ((not (ewd-linedef-back-sector linedef))
    (throw :hit t))
   ((and (not (eldoom-math-has-flag (emo-flags eph--tmthing) enf-MF_MISSILE))
         (or (eldoom-math-has-flag (ewd-linedef-flags linedef) enf-ML_BLOCKING)
             (and (not (emo-player eph--tmthing))
                  (eldoom-math-has-flag (ewd-linedef-flags linedef) enf-ML_BLOCKMONSTERS))))
    (throw :hit t))
   (t
    (let* ((front (ewd-linedef-front-sector linedef))
           (front-floor (ewd-sector-floor-height-fx front))
           (front-ceiling (ewd-sector-ceiling-height-fx front))
           (back (ewd-linedef-back-sector linedef))
           (back-floor (ewd-sector-floor-height-fx back))
           (back-ceiling (ewd-sector-ceiling-height-fx back)))
      (setq eph--tmceilingz-fx (min eph--tmceilingz-fx front-ceiling back-ceiling)
            eph--tmfloorz-fx (max eph--tmfloorz-fx front-floor back-floor)
            eph--tmdropoffz-fx (min eph--tmdropoffz-fx front-floor back-floor))
      (when (/= 0 (ewd-linedef-special linedef))
        (setf (ewd-linedef-next-hit linedef) eph-linedef-hit
              eph-linedef-hit linedef)))
    t)))

(defun eph--check-special (object special)
  "Part of PIT_CheckThing in p_map.c"
  (let ((solid (eldoom-math-has-flag (emo-flags special) enf-MF_SOLID)))
    (when (eldoom-math-has-flag (emo-flags object) enf-MF_PICKUP)
      (emo-touch-special special object))
    (when solid (throw :hit t))))

(defun eph--check-missile (missile target)
  "Part of PIT_CheckThing in p_map.c"
  (when (and (<= (emo-z-fx missile) (+ (emo-z-fx target) (emo-height-fx target)))
             (>= (+ (emo-z-fx missile) (emo-height-fx missile)) (emo-z-fx target)))
    (let ((shooter (emo-target missile)))
      (cond
       ((eql target shooter) nil)
       ((and shooter
             (or
              (= (emo-type shooter) (emo-type target))
              (and (= (emo-type shooter) enf-MT_KNIGHT)
                   (= (emo-type target) enf-MT_BRUISER))
              (and (= (emo-type shooter) enf-MT_BRUISER)
                   (= (emo-type target) enf-MT_KNIGHT))))
        (when (/= (emo-type target) enf-MT_PLAYER)
          (throw :hit t))) ;; don't damage the same type of monster
       ((not (eldoom-math-has-flag (emo-flags target) enf-MF_SHOOTABLE))
        (when (eldoom-math-has-flag (emo-flags target) enf-MF_SOLID)
          (throw :hit t)))
       (t
        (let ((damage (eldoom-random-dice (enf-mobj-info-damage (emo-info missile)) 8 t)))
          (emo-damage target missile shooter damage)
          (throw :hit t)))))))

(defun eph--check-skull (skull target)
  "Part of PIT_CheckThing in p_map.c"
  (let ((damage (eldoom-random-dice (enf-mobj-info-damage (emo-info skull)) 8 t)))
    (emo-damage target skull skull damage)
    (setf (emo-flags skull) (logand (emo-flags skull) (lognot enf-MF_SKULLFLY))
          (emo-momx skull) 0
          (emo-momy skull) 0
          (emo-momz skull) 0)
    (emo-set-state skull (enf-mobj-info-spawn-state (emo-info skull)))
    (throw :hit t)))

(defun eph--check-thing (mobj _)
  "Throw :hit with non-nil value if MOBJ not block the current test move.
PIT_CheckThing in p_map.c"
  (let* ((flags (emo-flags mobj))
         (block-dist (+ (emo-radius-fx mobj) (emo-radius-fx eph--tmthing))))
    (if (or
         (and (not (eldoom-math-has-flag flags enf-MF_SOLID))
              (not (eldoom-math-has-flag flags enf-MF_SPECIAL))
              (not (eldoom-math-has-flag flags enf-MF_SHOOTABLE)))
         (>= (abs (- (emo-x-fx mobj) eph--tmx-fx)) block-dist)
         (>= (abs (- (emo-y-fx mobj) eph--tmy-fx)) block-dist)
         (eql mobj eph--tmthing))
        t
      (cond
       ((eldoom-math-has-flag (emo-flags eph--tmthing) enf-MF_SKULLFLY)
        (eph--check-skull eph--tmthing mobj))
       ((eldoom-math-has-flag (emo-flags eph--tmthing) enf-MF_MISSILE)
        (eph--check-missile eph--tmthing mobj))
       ((eldoom-math-has-flag flags enf-MF_SPECIAL)
        (eph--check-special eph--tmthing mobj))
       (t
        (when (eldoom-math-has-flag flags enf-MF_SOLID) (throw :hit t)))))))

(defun eph-check-position (mobj x-fx y-fx)
  "P_CheckPosition in p_map.c.
Return non-nil if BODY can occupy fixed_t position X,Y."
  (when (not eph--tmbbox) (setq eph--tmbbox (eldoom-math-make-bbox)))
  (let* ((radius-fx (emo-radius-fx mobj))
         (subsector (emp-subsector-at x-fx y-fx))
         (sector (ewd-subsector-sector subsector)))
    (setf eph--tmthing mobj
          eph--tmx-fx x-fx
          eph--tmy-fx y-fx
          (eldoom-math-bbox-top eph--tmbbox) (+ y-fx radius-fx)
          (eldoom-math-bbox-bottom eph--tmbbox) (- y-fx radius-fx)
          (eldoom-math-bbox-right eph--tmbbox) (+ x-fx radius-fx)
          (eldoom-math-bbox-left eph--tmbbox) (- x-fx radius-fx)
          eph--tmfloorz-fx (ewd-sector-floor-height-fx sector)
          eph--tmdropoffz-fx eph--tmfloorz-fx
          eph--tmceilingz-fx (ewd-sector-ceiling-height-fx sector)
          eph-linedef-hit nil)
    (cl-incf eldoom-globals-iteration)
    (if (eldoom-math-has-flag (emo-flags mobj) enf-MF_NOCLIP)
        t
      (not
       (catch :hit
         (emo-for-each-in-block-overlapping-bbox (eldoom-math-bbox-left eph--tmbbox)
                                                 (eldoom-math-bbox-right eph--tmbbox)
                                                 (eldoom-math-bbox-bottom eph--tmbbox)
                                                 (eldoom-math-bbox-top eph--tmbbox)
                                                 #'eph--check-thing
                                                 nil)
         (emp-for-each-line-in-block-overlapping-bbox
          (eldoom-math-bbox-left eph--tmbbox)
          (eldoom-math-bbox-right eph--tmbbox)
          (eldoom-math-bbox-bottom eph--tmbbox)
          (eldoom-math-bbox-top eph--tmbbox)
          #'eph--check-line)
         nil ;; no hit
         )))))

(defvar-local eph-float-ok nil "If true, move would be ok if within \"tmfloorz - tmceilingz\".")

(defun eph--cross-special-line (linedef from-side mobj)
  "P_CrossSpecialLine in p_spec.c"
  (when-let* ((on-walk (ewd-linedef-on-walk linedef)))
    (funcall on-walk mobj from-side linedef)))

(defun eph--do-move (mobj x-fx y-fx)
  "P_TryMove (end part) in p_map.c"
  (emo-unset-position mobj)
  (let ((old-x (emo-x-fx mobj))
        (old-y (emo-y-fx mobj)))
    (setf (emo-floor-z mobj) eph--tmfloorz-fx
          (emo-ceiling-z mobj) eph--tmceilingz-fx
          (emo-x-fx mobj) x-fx
          (emo-y-fx mobj) y-fx)
    (emo-set-position mobj)
    (when (and (not (eldoom-math-has-flag (emo-flags mobj) enf-MF_TELEPORT))
               (not (eldoom-math-has-flag (emo-flags mobj) enf-MF_NOCLIP)))
      (let ((linedef eph-linedef-hit))
        (while linedef
          (let ((next-linedef nil)
                (side (eldoom-math-on-right-side-p (emo-x-fx mobj) (emo-y-fx mobj) (ewd-linedef-segment linedef)))
                (old-side (eldoom-math-on-right-side-p old-x old-y (ewd-linedef-segment linedef))))
            (when (and (not (eql side old-side))
                       (/= 0 (ewd-linedef-special linedef)))
              (eph--cross-special-line linedef old-side mobj))
            (setf next-linedef (ewd-linedef-next-hit linedef)
                  (ewd-linedef-next-hit linedef) nil
                  linedef next-linedef)))))))

(defun eph-try-move (mobj x-fx y-fx)
  "P_TryMove in p_map.c
Move MOBJ to fixed_t X,Y if legal; update x/y/z on success."
  (setq eph-float-ok nil)
  (let* ((z-fx (emo-z-fx mobj))
         (height-fx (emo-height-fx mobj))
         (flags (emo-flags mobj)))
    (when (eph-check-position mobj x-fx y-fx)
      (if (not (eldoom-math-has-flag flags enf-MF_NOCLIP))
          (progn
            (setq eph-float-ok (>= (- eph--tmceilingz-fx eph--tmfloorz-fx) height-fx))
            (if (or (not eph-float-ok) ;; doesn't fit
                    (and (not (eldoom-math-has-flag flags enf-MF_TELEPORT))
                         (< (- eph--tmceilingz-fx z-fx) height-fx)) ;; must lower itself to fit
                    (and (not (eldoom-math-has-flag flags enf-MF_TELEPORT))
                         (> (- eph--tmfloorz-fx z-fx) eph--max-step-height-fx)) ;; too big step up
                    (and (not (eldoom-math-has-flag flags enf-MF_DROPOFF))
                         (not (eldoom-math-has-flag flags enf-MF_FLOAT))
                         (> (- eph--tmfloorz-fx eph--tmdropoffz-fx) eph--max-step-height-fx))) ;; don't stand over a drop off
                nil
              (eph--do-move mobj x-fx y-fx)
              t))
        (eph--do-move mobj x-fx y-fx)
        t))))

(defun eph-check-missile-spawn (missile)
  "Moves the missile forward a bit and possibly explodes it right there.
P_CheckMissileSpawn in p_mobj.c"
  (setf (emo-tics missile) (max (- (emo-tics missile) (logand (eldoom-random t) 3)) 1))
  (cl-incf (emo-x-fx missile) (/ (emo-momx missile) 2))
  (cl-incf (emo-y-fx missile) (/ (emo-momy missile) 2))
  (cl-incf (emo-z-fx missile) (/ (emo-momz missile) 2))
  (when (not (eph-try-move missile (emo-x-fx missile) (emo-y-fx missile)))
    (emo-explode-missile missile)))

(cl-defstruct (emp--stomp-state (:constructor emp--make-stomp-state) (:copier nil))
  thing
  (x 0 :type fixnum) (y 0 :type fixnum))

(defun eph--stomp-thing (mobj stomp-state)
  "PIT_StompThing in p_map.c"
  (let* ((stomping-thing (emp--stomp-state-thing stomp-state))
         (stomping-x (emp--stomp-state-x stomp-state))
         (stomping-y (emp--stomp-state-y stomp-state))
         (block-dist (+ (emo-radius-fx mobj) (emo-radius-fx stomping-thing))))
    (when (and (eldoom-math-has-flag (emo-flags mobj) enf-MF_SHOOTABLE)
               (< (abs (- (emo-x-fx mobj) stomping-x)) block-dist)
               (< (abs (- (emo-y-fx mobj) stomping-y)) block-dist)
               (not (eq mobj stomping-thing)))
      (when (not (emo-player stomping-thing)) ;; and map is not 30
        (throw :blocked t))
      (emo-damage mobj stomping-thing stomping-thing 10000))))

(defvar-local emp--stomp-state nil)
(defun eph-teleport-move (mobj x y)
  "P_TeleportMove in p_map.c"
  (when (not emp--stomp-state) (setq emp--stomp-state (emp--make-stomp-state)))
  (setf (emp--stomp-state-thing emp--stomp-state) mobj
        (emp--stomp-state-x emp--stomp-state) x
        (emp--stomp-state-y emp--stomp-state) y)
  (cl-incf eldoom-globals-iteration) ;; is it really needed?
  (let* ((radius (emo-radius-fx mobj))
         (blocked (catch :blocked
                    (emo-for-each-in-block-overlapping-bbox (- x radius) (+ x radius)
                                                            (- y radius) (+ y radius)
                                                            #'eph--stomp-thing emp--stomp-state))))
    (when (not blocked)
      (emo-unset-position mobj)
      (let* ((new-ss (emp-subsector-at x y))
             (new-s (ewd-subsector-sector new-ss)))
        (setf (emo-floor-z mobj) (ewd-sector-floor-height-fx new-s)
              (emo-ceiling-z mobj) (ewd-sector-ceiling-height-fx new-s)
              (emo-x-fx mobj) x
              (emo-y-fx mobj) y))
      (emo-set-position mobj))
    t))

(defun eph--hit-slide-line (line mobj)
  "P_HitSlideLine in p_map.c"
  (cl-case (ewd-linedef-slope-type line)
    (:horizontal (setq eph--tmy-move 0))
    (:vertical (setq eph--tmx-move 0))
    (t
     (let ((side (eldoom-math-point-segment-orientation (emo-x-fx mobj) (emo-y-fx mobj)
                                                        (ewd-linedef-segment line)))
           (line-angle (eldoom-math-segment-angle (ewd-linedef-segment line)))
           (move-angle (eldoom-math-atan2 eph--tmy-move eph--tmx-move)))
       (when (eq side :left)
         (eldoom-math-angle-incf line-angle eldoom-math-ang180))
       (let ((delta-angle (eldoom-math-angle-sub move-angle line-angle)))
         (when (> delta-angle eldoom-math-ang180)
           (eldoom-math-angle-incf delta-angle eldoom-math-ang180))
         (let* ((move-len (emp-approx-distance 0 0 eph--tmx-move eph--tmy-move))
                (new-len (eldoom-math-mul move-len (eldoom-math-cos delta-angle))))
           (setq eph--tmx-move (eldoom-math-mul new-len (eldoom-math-cos line-angle))
                 eph--tmy-move (eldoom-math-mul new-len (eldoom-math-sin line-angle)))))))))

(defun eph--ptr-slide-traverse (intercept mobj)
  "PTR_SlideTraverse in p_map.c"
  (let ((line (etr-intercept-item intercept)))
    (if (eldoom-math-has-flag (ewd-linedef-flags line) enf-ML_TWOSIDED)
        (let* ((front (ewd-linedef-front-sector line))
               (front-floor (ewd-sector-floor-height-fx front))
               (front-ceiling (ewd-sector-ceiling-height-fx front))
               (back (ewd-linedef-back-sector line))
               (back-floor (ewd-sector-floor-height-fx back))
               (back-ceiling (ewd-sector-ceiling-height-fx back))
               (open-top (min front-ceiling back-ceiling))
               (open-bottom (max front-floor back-floor)))
          (when (or (< (- open-top open-bottom) (emo-height-fx mobj))
                    (< open-top (+ (emo-z-fx mobj) (emo-height-fx mobj)))
                    (> open-bottom (+ (emo-z-fx mobj) eph--max-step-height-fx)))
            (throw :blocking-intercept intercept)))
      (when (eq (eldoom-math-point-segment-orientation (emo-x-fx mobj) (emo-y-fx mobj)
                                                       (ewd-linedef-segment line))
                :right)
        (throw :blocking-intercept intercept)))))

(defun eph-slide-move (mobj)
  "P_SlideMove in p_map.c"
  (let ((loop-result 
         (catch :break
           (cl-loop repeat 2 do
                    (let* ((momx (emo-momx mobj))
                           (momy (emo-momy mobj))
                           (radius (emo-radius-fx mobj))
                           (lead-x (+ (emo-x-fx mobj) (if (> momx 0) radius (- radius))))
                           (trail-x (+ (emo-x-fx mobj) (if (> momx 0) (- radius) radius)))
                           (lead-y (+ (emo-y-fx mobj) (if (> momy 0) radius (- radius))))
                           (trail-y (+ (emo-y-fx mobj) (if (> momy 0) (- radius) radius)))
                           (best-slide-frac (1+ eldoom-math-unit))
                           (best-slide-line nil))
                      (when-let* ((intercept
                                   (catch :blocking-intercept
                                     (etr-path-traverse lead-x lead-y
                                                        (+ lead-x momx) (+ lead-y momy)
                                                        etr-pt-addlines #'eph--ptr-slide-traverse mobj)
                                     nil)))
                        (when (< (etr-intercept-frac intercept) best-slide-frac)
                          (setq best-slide-frac (etr-intercept-frac intercept)
                                best-slide-line (etr-intercept-item intercept))))
                      (when-let* ((intercept
                                   (catch :blocking-intercept
                                     (etr-path-traverse trail-x lead-y
                                                        (+ trail-x momx) (+ lead-y momy)
                                                        etr-pt-addlines #'eph--ptr-slide-traverse mobj)
                                     nil)))
                        (when (< (etr-intercept-frac intercept) best-slide-frac)
                          (setq best-slide-frac (etr-intercept-frac intercept)
                                best-slide-line (etr-intercept-item intercept))))
                      (when-let* ((intercept
                                   (catch :blocking-intercept
                                     (etr-path-traverse lead-x trail-y
                                                        (+ lead-x momx) (+ trail-y momy)
                                                        etr-pt-addlines #'eph--ptr-slide-traverse mobj)
                                     nil)))
                        (when (< (etr-intercept-frac intercept) best-slide-frac)
                          (setq best-slide-frac (etr-intercept-frac intercept)
                                best-slide-line (etr-intercept-item intercept))))
                      (when (= best-slide-frac (1+ eldoom-math-unit))
                        (throw :break :stair-step))
                      (cl-decf best-slide-frac #x800)
                      (when (> best-slide-frac 0)
                        (let ((new-x (eldoom-math-mul (emo-momx mobj) best-slide-frac))
                              (new-y (eldoom-math-mul (emo-momy mobj) best-slide-frac)))
                          (when (not (eph-try-move mobj (+ (emo-x-fx mobj) new-x)
                                                   (+ (emo-y-fx mobj) new-y)))
                            (throw :break :stair-step))))
                      (setq best-slide-frac (min (- eldoom-math-unit (+ best-slide-frac #x800)) eldoom-math-unit))
                      (when (<= best-slide-frac 0)
                        (throw :break nil))
                      (setq eph--tmx-move (eldoom-math-mul (emo-momx mobj) best-slide-frac)
                            eph--tmy-move (eldoom-math-mul (emo-momy mobj) best-slide-frac))
                      (eph--hit-slide-line best-slide-line mobj)
                      (setf (emo-momx mobj) eph--tmx-move
                            (emo-momy mobj) eph--tmy-move)
                      (when (eph-try-move mobj (+ (emo-x-fx mobj) eph--tmx-move)
                                          (+ (emo-y-fx mobj) eph--tmy-move))
                        (throw :break nil))))
           :stair-step)))
    (when (eq loop-result :stair-step)
      (when (not (eph-try-move mobj (emo-x-fx mobj) (+ (emo-y-fx mobj) (emo-momy mobj))))
        (eph-try-move mobj (+ (emo-x-fx mobj) (emo-momx mobj)) (emo-y-fx mobj))))))

(defconst eph--stop-speed #x1000)
(defconst eph--friction #xe800)
(defconst eph--max-move (eldoom-math-fixed-from-int 30))
(defconst eph--min-corpse-speed (/ eldoom-math-unit 4))

(defun eph--fast-sliding-corpse-p (mobj)
  (and (eldoom-math-has-flag (emo-flags mobj) enf-MF_CORPSE)
       (or (> (emo-momx mobj) eph--min-corpse-speed)
           (< (emo-momx mobj) (- eph--min-corpse-speed))
           (> (emo-momy mobj) eph--min-corpse-speed)
           (< (emo-momy mobj) (- eph--min-corpse-speed)))
       (/= (emo-floor-z mobj) (ewd-sector-floor-height-fx (ewd-subsector-sector (emo-subsector mobj))))))

(defun eph-xy-movement (mobj)
  "P_XYMovement in p_mobj.c"
  (if (and (zerop (emo-momx mobj)) (zerop (emo-momy mobj)))
      (when (eldoom-math-has-flag (emo-flags mobj) enf-MF_SKULLFLY)
        (eldoom-math-clear-flagf (emo-flags mobj) enf-MF_SKULLFLY)
        (setf (emo-x-fx mobj) 0
              (emo-y-fx mobj) 0
              (emo-z-fx mobj) 0)
        (emo-set-state mobj (enf-mobj-info-spawn-state (emo-info mobj))))
    (setf (emo-momx mobj) (eldoom-math-clamp (emo-momx mobj) (- eph--max-move) eph--max-move)
          (emo-momy mobj) (eldoom-math-clamp (emo-momy mobj) (- eph--max-move) eph--max-move))
    (let ((x-move (emo-momx mobj))
          (y-move (emo-momy mobj)))
      (while (or (/= 0 x-move) (/= 0 y-move))
        (let ((try-x (+ (emo-x-fx mobj) x-move))
              (try-y (+ (emo-y-fx mobj) y-move)))
          (if (or (> x-move (/ eph--max-move 2)) (> y-move (/ eph--max-move 2)))
              (setq try-x (+ (emo-x-fx mobj) (/ x-move 2))
                    try-y (+ (emo-y-fx mobj) (/ y-move 2))
                    x-move (/ x-move 2)
                    y-move (/ y-move 2))
            (setq x-move 0
                  y-move 0))
          (when (not (eph-try-move mobj try-x try-y))
            (if (emo-player mobj)
                (eph-slide-move mobj)
              (if (eldoom-math-has-flag (emo-flags mobj) enf-MF_MISSILE)
                  (if nil
                      nil ;; TODO: handle case of shooting into the sky
                    (emo-explode-missile mobj))
                (setf (emo-momx mobj) 0
                      (emo-momy mobj) 0)))))))
    ;; apply friction
    (if (and (emo-player mobj)
             (eldoom-math-has-flag (emo-player-cheats (emo-player mobj)) eldoom-globals-CF_NOMOMENTUM))
        (setf (emo-momx mobj) 0
              (emo-momy mobj) 0)
      (let ((flags (emo-flags mobj)))
        (when (and (not (eldoom-math-has-flag flags enf-MF_MISSILE))
                   (not (eldoom-math-has-flag flags enf-MF_SKULLFLY))
                   (<= (emo-z-fx mobj) (emo-floor-z mobj))
                   ;; don't apply friction to corpse sliding over a step with some momentum
                   (not (eph--fast-sliding-corpse-p mobj)))
          (if (and (< (- eph--stop-speed) (emo-momx mobj) eph--stop-speed)
                   (< (- eph--stop-speed) (emo-momy mobj) eph--stop-speed)
                   (not (emo-player mobj))) ;; TODO: or player with no command move
              (progn
                (when (emo-player mobj)
                  (let ((state (emo-state mobj)))
                    (when (or (eq state (aref enf-states enf-S_PLAY_RUN1))
                              (eq state (aref enf-states enf-S_PLAY_RUN2))
                              (eq state (aref enf-states enf-S_PLAY_RUN3))
                              (eq state (aref enf-states enf-S_PLAY_RUN4)))
                      (emo-set-state mobj (aref enf-states enf-S_PLAY)))))
                (setf (emo-momx mobj) 0
                      (emo-momy mobj) 0))
            (setf (emo-momx mobj) (eldoom-math-mul (emo-momx mobj) eph--friction)
                  (emo-momy mobj) (eldoom-math-mul (emo-momy mobj) eph--friction))))))))

(defun eph-z-movement (mobj)
  "P_ZMovement in p_mobj.c"
  (when-let* ((player (emo-player mobj)))
    (when (< (emo-z-fx mobj) (emo-floor-z mobj))
      (cl-decf (emo-player-view-height player) (- (emo-floor-z mobj) (emo-z-fx mobj)))
      (setf (emo-player-delta-view-height player) (ash (- emo-view-height (emo-player-view-height player)) -3))))
  (cl-incf (emo-z-fx mobj) (emo-momz mobj))
  (let ((flags (emo-flags mobj)))
    (when (and (eldoom-math-has-flag (emo-flags mobj) enf-MF_FLOAT)
               (emo-target mobj)
               (not (eldoom-math-has-flag flags enf-MF_SKULLFLY))
               (not (eldoom-math-has-flag flags enf-MF_INFLOAT)))
      (let* ((target (emo-target mobj))
             (dist (emp-approx-distance (emo-x-fx mobj) (emo-y-fx mobj) (emo-x-fx target) (emo-y-fx target)))
             (delta (- (+ (emo-z-fx target) (/ (emo-height-fx mobj) 2)) (emo-z-fx mobj))))
        (cond
         ((and (< delta 0) (< dist (* delta -3))) (cl-decf (emo-z-fx mobj) emo-float-speed))
         ((and (> delta 0) (< dist (* delta 3))) (cl-incf (emo-z-fx mobj) emo-float-speed)))))
    (catch :break
      (cond
       ((<= (emo-z-fx mobj) (emo-floor-z mobj))
        (when (eldoom-math-has-flag flags enf-MF_SKULLFLY)
          (setf (emo-momz mobj) (- (emo-momz mobj))))
        (when (< (emo-momz mobj) 0)
          (when (and (emo-player mobj)
                     (< (emo-momz mobj) (* emo-gravity -8)))
            (setf (emo-player-delta-view-height (emo-player mobj)) (ash (emo-momz mobj) -3)))
          (setf (emo-momz mobj) 0))
        (setf (emo-z-fx mobj) (emo-floor-z mobj))
        (when (and (eldoom-math-has-flag flags enf-MF_MISSILE)
                   (not (eldoom-math-has-flag flags enf-MF_NOCLIP)))
          (emo-explode-missile mobj)
          (throw :break t)))
       ((not (eldoom-math-has-flag flags enf-MF_NOGRAVITY))
        (if (zerop (emo-momz mobj))
            (setf (emo-momz mobj) (* emo-gravity -2))
          (cl-decf (emo-momz mobj) emo-gravity))))
      (when (> (+ (emo-z-fx mobj) (emo-height-fx mobj)) (emo-ceiling-z mobj))
        (when (> (emo-momz mobj) 0)
          (setf (emo-momz mobj) 0))
        (setf (emo-z-fx mobj) (- (emo-ceiling-z mobj) (emo-height-fx mobj)))
        (when (eldoom-math-has-flag flags enf-MF_SKULLFLY)
          (setf (emo-momz mobj) (- (emo-momz mobj))))
        (when (and (eldoom-math-has-flag flags enf-MF_MISSILE)
                   (not (eldoom-math-has-flag flags enf-MF_NOCLIP)))
          (emo-explode-missile mobj))))))

;; Local Variables:
;; read-symbol-shorthands: (("eph-" . "eldoom-phys-") ("emo-" . "eldoom-mobj-") ("ewd-" . "eldoom-wad-") ("emp-" . "eldoom-map-") ("enf-" . "eldoom-info-") ("etr-" . "eldoom-trace-"))
;; End:
(provide 'eldoom-phys)
;;; eldoom-phys.el ends here
