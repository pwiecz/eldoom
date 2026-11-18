;;; eldoom-actor.el --- Monster AI and projectiles -*- lexical-binding:t -*-

;; Copyright (C) 2026 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Chase movement (P_NewChaseDir-style), hitscan damage, and
;; projectile thinkers built on eldoom-phys and eldoom-trace.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'eldoom-globals)
(require 'eldoom-info)
(require 'eldoom-map)
(require 'eldoom-math)
(require 'eldoom-mobj)
(require 'eldoom-phys)
(require 'eldoom-sector)
(require 'eldoom-sight)
(require 'eldoom-think)
(require 'eldoom-trace)
(require 'eldoom-wad)

(defun eac--recursive-sound (target sector blocks)
  "P_RecursiveSound in p_enemy.c"
  (when (or (/= (ewd-sector-iteration sector) eldoom-globals-iteration)
            (> (ewd-sector-sound-traversed sector) (1+ blocks)))
    (setf (ewd-sector-iteration sector) eldoom-globals-iteration
          (ewd-sector-sound-traversed sector) (1+ blocks)
          (ewd-sector-sound-target sector) target)
    (cl-loop for line across (ewd-sector-linedefs sector) do
             (when (eldoom-math-has-flag (ewd-linedef-flags line) enf-ML_TWOSIDED)
               (let* ((front (ewd-linedef-front-sector line))
                      (back (ewd-linedef-back-sector line))
                      (top (min (ewd-sector-ceiling-height-fx front)
                                (ewd-sector-ceiling-height-fx back)))
                      (bottom (max (ewd-sector-floor-height-fx front)
                                   (ewd-sector-floor-height-fx back))))
                 (when (> top bottom)
                   (let ((other (emp-next-sector line sector)))
                     (if (eldoom-math-has-flag (ewd-linedef-flags line) enf-ML_SOUNDBLOCK)
                         (when (zerop blocks)
                           (eac--recursive-sound target other 1))
                       (eac--recursive-sound target other blocks)))))))))

(defun eac-noise-alert (target emitter)
  "P_NoiseAlert in p_enemy.c"
  (cl-incf eldoom-globals-iteration)
  (eac--recursive-sound target (ewd-subsector-sector (emo-subsector emitter)) 0))

(defun eac--close-or-in-front-p (actor target)
  (let* ((actor-x (emo-x-fx actor))
         (actor-y (emo-y-fx actor))
         (target-x (emo-x-fx target))
         (target-y (emo-y-fx target))
         (angle (eldoom-math-angle-sub (eldoom-math-atan2 (- target-y actor-y)
                                                          (- target-x actor-x))
                                       (emo-angle actor))))
    (or (<= angle eldoom-math-ang90)
        (<= eldoom-math-ang270 angle)
        (<= (emp-approx-distance actor-x actor-y target-x target-y)
            eldoom-globals-melee-range))))

(defun eac--look-for-player (actor all-around)
  "P_LookForPlayers in p_enemy.c
If all-around is false, only look 180 degrees in front.
Returns true if a player is targeted."
  (when (and (> (emo-health emo-player-mobj) 0)
             (eldoom-sight-check actor emo-player-mobj)
             (or all-around
                 (eac--close-or-in-front-p actor emo-player-mobj)))
    (setf (emo-target actor) emo-player-mobj)
    t))

(defconst eac--x-speed (vector eldoom-math-unit 47000 0 -47000
                               (- eldoom-math-unit) -47000 0 47000))
(defconst eac--y-speed (vector 0 47000 eldoom-math-unit 47000
                               0 -47000 (- eldoom-math-unit) -47000))

(defconst eac--DI_EAST 0)
(defconst eac--DI_NORTHEAST 1)
(defconst eac--DI_NORTH 2)
(defconst eac--DI_NORTHWEST 3)
(defconst eac--DI_WEST 4)
(defconst eac--DI_SOUTHWEST 5)
(defconst eac--DI_SOUTH 6)
(defconst eac--DI_SOUTHEAST 7)
(defconst eac--DI_NODIR 8)

(defun eac--move (mobj)
  "P_Move in p_enemy.c"
  (when (/= (emo-move-direction mobj) eac--DI_NODIR)
    (let* ((dir (emo-move-direction mobj))
           (info (emo-info mobj))
           (try-x (+ (emo-x-fx mobj) (eldoom-math-mul (enf-mobj-info-speed-fx info)
                                                      (aref eac--x-speed dir))))
           (try-y (+ (emo-y-fx mobj) (eldoom-math-mul (enf-mobj-info-speed-fx info)
                                                      (aref eac--y-speed dir)))))
      (if (eph-try-move mobj try-x try-y)
          (progn
            (eldoom-math-clear-flagf (emo-flags mobj) enf-MF_INFLOAT)
            (when (not (eldoom-math-has-flag (emo-flags mobj) enf-MF_FLOAT))
              (setf (emo-z-fx mobj) (emo-floor-z mobj)))
            t)
        (if (and eph-float-ok
                 (eldoom-math-has-flag (emo-flags mobj) enf-MF_FLOAT))
            (progn
              (if (< (emo-z-fx mobj) eph--tmfloorz-fx)
                  (cl-incf (emo-z-fx mobj) emo-float-speed)
                (cl-decf (emo-z-fx mobj) emo-float-speed))
              (eldoom-math-set-flagf (emo-flags mobj) enf-MF_INFLOAT)
              t)
          (when-let* ((linedef eph-linedef-hit))
            (setf (emo-move-direction mobj) eac--DI_NODIR)
            (let ((next-linedef nil)
                  (good nil))
              (while linedef
                (when (ese-use-special-line mobj linedef)
                  (setq good t))
                (setf next-linedef (ewd-linedef-next-hit linedef)
                      (ewd-linedef-next-hit linedef) nil
                      linedef next-linedef))
              good)))))))

(defun eac--try-walk (mobj)
  "P_TryWalk in p_enemy.c"
  (when (eac--move mobj)
    (setf (emo-move-count mobj) (logand (eldoom-random t) 15))
    t))

(defun eac--try-dir (mobj dir)
  (declare (ftype (function (t fixnum) t)))
  (setf (emo-move-direction mobj) dir)
  (when (eac--try-walk mobj)
    (throw :done nil)))

(defconst eac--opposite (vector eac--DI_WEST eac--DI_SOUTHWEST eac--DI_SOUTH
                                eac--DI_SOUTHEAST eac--DI_EAST eac--DI_NORTHEAST
                                eac--DI_NORTH eac--DI_NORTHWEST eac--DI_NODIR))
(defconst eac--diagonals (vector eac--DI_NORTHWEST eac--DI_NORTHEAST eac--DI_SOUTHWEST
                                 eac--DI_SOUTHEAST))

(defun eac--new-chase-dir (mobj)
  "P_NewChaseDir in p_enemy.c"
  (cl-assert (emo-target mobj))
  (let* ((old-dir (emo-move-direction mobj))
         (turn-around (aref eac--opposite old-dir))
         (t-dir eac--DI_NODIR)
         (delta-x (- (emo-x-fx (emo-target mobj)) (emo-x-fx mobj)))
         (delta-y (- (emo-y-fx (emo-target mobj)) (emo-y-fx mobj)))
         (d1 (cond
              ((> delta-x (eldoom-math-fixed-from-int 10)) eac--DI_EAST)
              ((< delta-x (eldoom-math-fixed-from-int -10)) eac--DI_WEST)
              (t eac--DI_NODIR)))
         (d2 (cond
              ((< delta-y (eldoom-math-fixed-from-int -10)) eac--DI_SOUTH)
              ((> delta-y (eldoom-math-fixed-from-int 10)) eac--DI_NORTH)
              (t eac--DI_NODIR))))
    (catch :done
      (when (and (/= d1 eac--DI_NODIR) (/= d2 eac--DI_NODIR))
        (let* ((direct-index (+ (if (< delta-y 0) 2 0) (if (> delta-x 0) 1 0)))
               (direct-dir (aref eac--diagonals direct-index)))
          (when (/= direct-dir turn-around)
            (eac--try-dir mobj direct-dir))))
      (when (or (> (eldoom-random t) 200)
                (> (abs delta-y) (abs delta-x)))
        (cl-rotatef t-dir d1 d2))
      (when (= d1 turn-around)
        (setq d1 eac--DI_NODIR))
      (when (= d2 turn-around)
        (setq d2 eac--DI_NODIR))
      (when (/= d1 eac--DI_NODIR)
        (eac--try-dir mobj d1))
      (when (/= d2 eac--DI_NODIR)
        (eac--try-dir mobj d2))
      (when (/= old-dir eac--DI_NODIR)
        (eac--try-dir mobj old-dir))
      (if (/= 0 (logand (eldoom-random t) 1))
          (cl-loop for tdir from eac--DI_EAST to eac--DI_SOUTHEAST do
                   (when (/= tdir turn-around)
                     (eac--try-dir mobj tdir)))
        (cl-loop for tdir downfrom eac--DI_SOUTHEAST downto eac--DI_EAST do
                 (when (/= tdir turn-around)
                   (eac--try-dir mobj tdir))))
      (when (/= turn-around eac--DI_NODIR)
        (eac--try-dir mobj turn-around))
      (setf (emo-move-direction mobj) eac--DI_NODIR))))

(defun eac--check-melee-range (mobj)
  "P_CheckMeleeRange in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (let ((dist (emp-approx-distance (emo-x-fx mobj) (emo-y-fx mobj)
                                     (emo-x-fx target) (emo-y-fx target))))
      (and (< dist (+ (- eldoom-globals-melee-range (eldoom-math-fixed-from-int 20))
                      (emo-radius-fx target)))
           (eldoom-sight-check mobj target)))))

(defun eac--check-missile-range (mobj)
  "P_CheckMissileRange in p_enemy.c"
  (when (eldoom-sight-check mobj (emo-target mobj))
    (catch :done
      (when (eldoom-math-has-flag (emo-flags mobj) enf-MF_JUSTHIT)
        (eldoom-math-clear-flagf (emo-flags mobj) enf-MF_JUSTHIT)
        (throw :done t))
      (when (/= 0 (emo-reaction-time mobj))
        (throw :done nil))
      (let ((dist (- (emp-approx-distance (emo-x-fx (emo-target mobj)) (emo-y-fx (emo-target mobj))
                                          (emo-x-fx mobj) (emo-y-fx mobj))
                     (eldoom-math-fixed-from-int 64))))
        (when (not (enf-mobj-info-melee-state (emo-info mobj)))
          (cl-decf dist (eldoom-math-fixed-from-int 128))) ;; no melee attack, so fire further
        (setq dist (ash dist -16))
        (let ((type (emo-type mobj)))
          (cond
           ((= type enf-MT_VILE)
            (when (> dist (* 14 64)) (throw :done nil))) ;; too far away
           ((= type enf-MT_UNDEAD)
            (if (< dist 196) (throw :done nil) (setq dist (ash dist -1)))) ;; too close
           ((or (= type enf-MT_CYBORG)
                (= type enf-MT_SPIDER)
                (= type enf-MT_SKULL))
            (setq dist (ash dist -1))))
          (setq dist (min dist 200))
          (when (and (= type enf-MT_CYBORG) (> dist 160))
            (setq dist 160))
          (throw :done (>= (eldoom-random t) dist)))))))

(defun eac--face-target (mobj target)
  (eldoom-math-clear-flagf (emo-flags mobj) enf-MF_AMBUSH)
  (setf (emo-angle mobj) (eldoom-math-atan2 (- (emo-y-fx target) (emo-y-fx mobj))
                                            (- (emo-x-fx target) (emo-x-fx mobj))))
  (when (eldoom-math-has-flag (emo-flags target) enf-MF_SHADOW)
    (let ((angle-delta (ash (- (eldoom-random t) (eldoom-random t)) 21)))
      (eldoom-math-angle-incf (emo-angle mobj) angle-delta))))

(defun eac--a-face-target (mobj)
  "A_FaceTarget in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eac--face-target mobj target)))

(defun eac--a-bspi-attack (mobj)
  "A_BspiAttack in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eac--face-target mobj target)
    (emo-spawn-missile mobj target enf-MT_ARACHPLAZ 0)))

(defun eac--a-troop-attack (mobj)
  "A_TroopAttack in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eac--face-target mobj target)
    (if (eac--check-melee-range mobj)
        (let ((damage (eldoom-random-dice 3 8 t)))
          (emo-damage target mobj mobj damage))
      (let ((missile (emo-spawn-missile mobj target enf-MT_TROOPSHOT 0)))
        (eph-check-missile-spawn missile)))))

(defun eac--a-pos-attack (mobj)
  "A_PosAttack in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eac--face-target mobj target)
    (let* ((angle (emo-angle mobj))
           (aim-result (etr-aim-line-attack mobj angle eldoom-globals-missile-range))
           (slope (etr-aim-result-slope aim-result)))
      (cl-incf angle (ash (- (eldoom-random t) (eldoom-random t)) 20))
      (let ((damage (eldoom-random-dice 3 5 t)))
        (etr-shoot mobj angle eldoom-globals-missile-range slope damage)))))

(defun eac--a-spos-attack (mobj)
  "A_SPosAttack in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eac--face-target mobj target)
    (let* ((base-angle (emo-angle mobj))
           (aim-result (etr-aim-line-attack mobj base-angle eldoom-globals-missile-range))
           (slope (etr-aim-result-slope aim-result)))
      (cl-loop repeat 3 do
               (let ((angle (+ base-angle (ash (- (eldoom-random t) (eldoom-random t)) 20)))
                     (damage (eldoom-random-dice 3 5 t)))
                 (etr-shoot mobj angle eldoom-globals-missile-range slope damage))))))

(defun eac--a-cpos-attack (mobj)
  "A_CPosAttack in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eac--face-target mobj target)
    (let* ((angle (emo-angle mobj))
           (aim-result (etr-aim-line-attack mobj angle eldoom-globals-missile-range))
           (slope (etr-aim-result-slope aim-result)))
      (cl-incf angle (ash (- (eldoom-random t) (eldoom-random t)) 20))
      (let ((damage (eldoom-random-dice 3 5 t)))
        (etr-shoot mobj angle eldoom-globals-missile-range slope damage)))))

(defun eac--a-cpos-refire (mobj)
  "A_CPosRefire in p_enemy.c"
  (eac--a-face-target mobj)
  (when (>= (eldoom-random t) 40)
    (let ((target (emo-target mobj)))
      (when (or (not target)
                (<= (emo-health target) 0)
                (not (eldoom-sight-check mobj target)))
        (emo-set-state mobj (enf-mobj-info-see-state (emo-info mobj)))))))

(defun eac--a-spid-refire (mobj)
  "A_SpidRefire in p_enemy.c"
  (eac--a-face-target mobj)
  (when (>= (eldoom-random t) 10)
    (let ((target (emo-target mobj)))
      (when (or (not target)
                (<= (emo-health target) 0)
                (not (eldoom-sight-check mobj target)))
        (emo-set-state mobj (enf-mobj-info-see-state (emo-info mobj)))))))

(defvar-local eac--lost-soul-count 0)
(defun eac--count-lost-souls-callback (thinker _obj)
  (when (and (emo-p (eth-thinker-object thinker))
             (= (emo-type (eth-thinker-object thinker)) enf-MT_SKULL))
    (cl-incf eac--lost-soul-count)))

(defun eac--count-lost-souls ()
  (setq eac--lost-soul-count 0)
  (eth-for-each-thinker #'eac--count-lost-souls-callback nil)
  eac--lost-soul-count)

(defun eac--a-skull-attack (mobj)
  "A_SkullAttack in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eldoom-math-set-flagf (emo-flags mobj) enf-MF_SKULLFLY)
    (eac--face-target mobj target)
    (let* ((speed eldoom-globals-skull-speed)
           (dist (max (/ (emp-approx-distance (emo-x-fx mobj) (emo-y-fx mobj)
                                              (emo-x-fx target) (emo-y-fx target))
                         speed)
                      1)))
      (setf (emo-momx mobj) (eldoom-math-mul speed (eldoom-math-cos (emo-angle mobj)))
            (emo-momy mobj) (eldoom-math-mul speed (eldoom-math-sin (emo-angle mobj)))
            (emo-momz mobj) (/ (- (+ (emo-z-fx target) (/ (emo-height-fx target) 2))
                                  (emo-z-fx mobj))
                               dist)))))

(defun eac--a-pain-shoot-skull (mobj angle)
  "A_PainShootSkull in p_enemy.c"
  (when (<= (eac--count-lost-souls) 20)
    (let* ((pain-elemental-radius (enf-mobj-info-radius-fx (emo-info mobj)))
           (lost-soul-radius (enf-mobj-info-radius-fx (aref enf-mobj-infos enf-MT_SKULL)))
           (prestep (+ (eldoom-math-fixed-from-int 4)
                       (* 3 (/ (+ pain-elemental-radius lost-soul-radius) 2))))
           (x (+ (emo-x-fx mobj) (eldoom-math-mul prestep (eldoom-math-cos angle))))
           (y (+ (emo-y-fx mobj) (eldoom-math-mul prestep (eldoom-math-sin angle))))
           (z (+ (emo-z-fx mobj) (eldoom-math-fixed-from-int 8)))
           (lost-soul-mobj (emo-spawn-one x y z enf-MT_SKULL)))
      (if (eph-try-move lost-soul-mobj x y)
          (progn
            (setf (emo-target lost-soul-mobj) (emo-target mobj))
            (eac--a-skull-attack lost-soul-mobj))
        (emo-damage lost-soul-mobj mobj mobj 10000)))))

(defun eac--a-pain-attack (mobj)
  "A_PainAttack in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eac--face-target mobj target)
    (eac--a-pain-shoot-skull mobj (emo-angle mobj))))

(defun eac--a-chase (mobj)
  "A_Chase in p_enemy.c"
  (let ((target (emo-target mobj)))
    (when (/= 0 (emo-reaction-time mobj))
      (cl-decf (emo-reaction-time mobj)))
    (when (/= 0 (emo-threshold mobj))
      (if (or (not target)
              (<= (emo-health target) 0))
          (setf (emo-threshold mobj) 0)
        (cl-decf (emo-threshold mobj))))
    (when (< (emo-move-direction mobj) 8)
      (setf (emo-angle mobj) (logand (emo-angle mobj) (ash 7 29)))
      (let ((delta (- (emo-angle mobj) (ash (emo-move-direction mobj) 29))))
        (cond
         ((> delta 0)
          (eldoom-math-angle-decf (emo-angle mobj) eldoom-math-ang45))
         ((< delta 0)
          (eldoom-math-angle-incf (emo-angle mobj) eldoom-math-ang45)))))
    (cond
     ((or (not target)
          (not (eldoom-math-has-flag (emo-flags target) enf-MF_SHOOTABLE)))
      (when (not (eac--look-for-player mobj t))
        (emo-set-state mobj (enf-mobj-info-spawn-state (emo-info mobj)))))
     ((eldoom-math-has-flag (emo-flags mobj) enf-MF_JUSTATTACKED)
      (eldoom-math-clear-flagf (emo-flags mobj) enf-MF_JUSTATTACKED)
      (when (and (not (eq eldoom-globals-difficulty :nightmare))
                 (not eldoom-globals-fast-monsters))
        (eac--new-chase-dir mobj)))
     ((and (enf-mobj-info-melee-state (emo-info mobj))
           (eac--check-melee-range mobj))
      (emo-set-state mobj (enf-mobj-info-melee-state (emo-info mobj))))
     ((and (enf-mobj-info-missile-state (emo-info mobj))
           (or (eq eldoom-globals-difficulty :nightmare)
               eldoom-globals-fast-monsters
               (zerop (emo-move-count mobj)))
           (eac--check-missile-range mobj))
      (emo-set-state mobj (enf-mobj-info-missile-state (emo-info mobj)))
      (eldoom-math-set-flagf (emo-flags mobj) enf-MF_JUSTATTACKED))
     (t
      (cl-decf (emo-move-count mobj))
      (when (or (< (emo-move-count mobj) 0)
                (not (eac--move mobj)))
        (eac--new-chase-dir mobj))))))

(defun eac--a-vile-start (_mobj)
  "A_VileStart in p_enemy.c"
  ;; just emits a sound
  )
(defun eac--a-vile-target (mobj)
  "A_VileTarget in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eac--face-target mobj target)
    (let ((fire (emo-spawn-one (emo-x-fx target) (emo-y-fx target) (emo-z-fx target)
                               enf-MT_FIRE)))
      (setf (emo-tracer mobj) fire
            (emo-target fire) mobj
            (emo-tracer fire) target)
      (eac--a-fire fire))))

(defun eac--a-vile-attack (mobj)
  "A_VileAttack in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eac--face-target mobj target)
    (when (eldoom-sight-check mobj target)
      (emo-damage target mobj mobj 20)
      (setf (emo-momz target) (/ (eldoom-math-fixed-from-int 1000)
                                 (enf-mobj-info-mass (emo-info target))))
      (when-let* ((fire (emo-tracer mobj)))
        (setf (emo-x-fx fire) (- (emo-x-fx target)
                                 (eldoom-math-mul (eldoom-math-fixed-from-int 24)
                                                  (eldoom-math-cos (emo-angle mobj))))
              (emo-y-fx fire) (- (emo-y-fx target)
                                 (eldoom-math-mul (eldoom-math-fixed-from-int 24)
                                                  (eldoom-math-sin (emo-angle mobj)))))
        (etr-radius-attack fire mobj 70)))))

(defun eac--a-fire (mobj)
  "A_Fire in p_enemy.c"
  (when-let* ((dest (emo-tracer mobj)))
    (when (eldoom-sight-check (emo-target mobj) dest)
      (emo-unset-position mobj)
      (setf (emo-x-fx mobj) (+ (emo-x-fx dest)
                               (eldoom-math-mul (eldoom-math-fixed-from-int 24)
                                                (eldoom-math-cos (emo-angle dest))))
            (emo-y-fx mobj) (+ (emo-y-fx dest)
                               (eldoom-math-mul (eldoom-math-fixed-from-int 24)
                                                (eldoom-math-sin (emo-angle dest))))
            (emo-z-fx mobj) (emo-z-fx dest))
      (emo-set-position mobj))))

(defun eac--a-start-fire (mobj)
  "A_StartFire in p_enemy.c"
  (eac--a-fire mobj))
(defun eac--a-fire-crackle (mobj)
  "A_FireCrackle in p_enemy.c"
  (eac--a-fire mobj))

(cl-defstruct (eac--vile-check-state (:constructor eac--make-vile-check-state)
                                     (:copier nil))
  (try-x 0 :type fixnum)
  (try-y 0 :type fixnum)
  obj)

(defun eac--vile-check-callback (mobj check-state)
  "PIT_VileCheck in p_enemy.c"
  (when (and (eldoom-math-has-flag (emo-flags mobj) enf-MF_CORPSE)
             (= (emo-tics mobj) -1)
             (enf-mobj-info-raise-state (emo-info mobj)))
    (let ((max-dist (+ (enf-mobj-info-radius-fx (emo-info mobj))
                       (enf-mobj-info-radius-fx (aref enf-states enf-MT_VILE)))))
      (when (and (<= (abs (- (emo-x-fx mobj) (eac--vile-check-state-try-x check-state)))
                     max-dist)
                 (<= (abs (- (emo-y-fx mobj) (eac--vile-check-state-try-y check-state)))
                     max-dist))
        (setf (emo-momx mobj) 0
              (emo-momy mobj) 0
              (emo-height-fx mobj) (* 4 (emo-height-fx mobj)))
        (let ((res (eph-check-position mobj (emo-x-fx mobj) (emo-y-fx mobj))))
          (setf (emo-height-fx mobj) (/ (emo-height-fx mobj) 4))
          (when res
            (throw :corpse-hit mobj)))))))

(defvar-local eac--vile-check-state nil)
(defun eac--a-vile-chase (mobj)
  "A_VileChase in p_enemy.c"
  (unless eac--vile-check-state (setq eac--vile-check-state (eac--make-vile-check-state)))
  (when-let* 
      ((corpse-hit
        (catch :corpse-hit
          (when (/= (emo-move-direction mobj) eac--DI_NODIR)
            (let* ((try-x (+ (emo-x-fx mobj)
                             (eldoom-math-mul (enf-mobj-info-speed-fx (emo-info mobj))
                                              (aref eac--x-speed (emo-move-direction mobj)))))
                   (try-y (+ (emo-y-fx mobj)
                             (eldoom-math-mul (enf-mobj-info-speed-fx (emo-info mobj))
                                              (aref eac--y-speed (emo-move-direction mobj))))))
              (setf (eac--vile-check-state-try-x eac--vile-check-state) try-x
                    (eac--vile-check-state-try-y eac--vile-check-state) try-y
                    (eac--vile-check-state-obj eac--vile-check-state) mobj)
              (emo-for-each-in-block-overlapping-bbox                 
               (- try-x ewd-max-radius) (+ try-x ewd-max-radius)
               (- try-y ewd-max-radius) (+ try-y ewd-max-radius)
               #'eac--vile-check-callback eac--vile-check-state)
              (eac--a-chase mobj)
              nil))))
       (corpse-info (emo-info corpse-hit)))
    (eac--face-target mobj corpse-hit)
    (emo-set-state mobj (aref enf-states enf-S_VILE_HEAL1))
    (emo-set-state corpse-hit (enf-mobj-info-raise-state corpse-info))
    (setf (emo-height-fx corpse-hit) (* (emo-height-fx corpse-hit) 4)
          (emo-flags corpse-hit) (enf-mobj-info-flags corpse-info)
          (emo-health corpse-hit) (enf-mobj-info-spawn-health corpse-info)
          (emo-target corpse-hit) nil)))

(defun eac--a-sarg-attack (mobj)
  "A_SargAttack in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eac--face-target mobj target)
    (when (eac--check-melee-range mobj)
      (let ((damage (eldoom-random-dice 4 10 t)))
        (emo-damage target mobj mobj damage)))))

(defun eac--a-head-attack (mobj)
  "A_HeadAttack in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eac--face-target mobj target)
    (if (eac--check-melee-range mobj)
        (let ((damage (eldoom-random-dice 10 6 t)))
          (emo-damage target mobj mobj damage))
      (emo-spawn-missile mobj target enf-MT_HEADSHOT 0))))

(defun eac--a-cyber-attack (mobj)
  "A_CyberAttack in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eac--a-face-target mobj)
    (emo-spawn-missile mobj target enf-MT_ROCKET 0)))

(defun eac--a-bruis-attack (mobj)
  "A_BruisAttack in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (if (eac--check-melee-range mobj)
        (let ((damage (eldoom-random-dice 10 8 t)))
          (emo-damage target mobj mobj damage))
      (emo-spawn-missile mobj target enf-MT_BRUISERSHOT 0))))

(defun eac--a-skel-missile (mobj)
  "A_SkelMissile in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eac--face-target mobj target)
    ;; raise temporarily the z coordinate to make missile spawn higher
    (cl-incf (emo-z-fx mobj) (eldoom-math-fixed-from-int 16))
    (let ((missile (emo-spawn-missile mobj target enf-MT_TRACER 0)))
      (cl-incf (emo-x-fx missile) (emo-momx missile))
      (cl-incf (emo-y-fx missile) (emo-momy missile))
      (setf (emo-tracer missile) target))
    (cl-decf (emo-z-fx mobj) (eldoom-math-fixed-from-int 16))))

(defconst eac--trace-angle #xc000000)

(defun eac--a-tracer (mobj)
  "A_Tracer in p_enemy.c"
  (when (zerop (logand eldoom-globals-game-tic 3))
    (emo-spawn-puff (emo-x-fx mobj) (emo-y-fx mobj) (emo-z-fx mobj) nil)
    (let ((smoke (emo-spawn-one (- (emo-x-fx mobj) (emo-momx mobj))
                                (- (emo-y-fx mobj) (emo-momy mobj))
                                (emo-z-fx mobj) enf-MT_SMOKE)))
      (setf (emo-momz smoke) eldoom-math-unit
            (emo-tics smoke) (max 1 (- (emo-tics smoke) (logand (eldoom-random t) 3)))))
    (let ((dest (emo-tracer mobj)))
      (when (and dest (> (emo-health dest) 0))
        (let ((angle (eldoom-math-atan2 (- (emo-y-fx dest) (emo-y-fx mobj))
                                        (- (emo-x-fx dest) (emo-x-fx mobj))))
              (speed (enf-mobj-info-speed-fx (emo-info mobj))))
          (when (/= angle (emo-angle mobj))
            (if (> (eldoom-math-angle-sub angle (emo-angle mobj)) eldoom-math-ang180)
                (progn
                  (eldoom-math-angle-decf (emo-angle mobj) eac--trace-angle)
                  (when (< (eldoom-math-ang (- angle (emo-angle mobj))) eldoom-math-ang180)
                    (setf (emo-angle mobj) angle)))
              (eldoom-math-angle-incf (emo-angle mobj) eac--trace-angle)
              (when (> (eldoom-math-angle-sub angle (emo-angle mobj)) eldoom-math-ang180)
                (setf (emo-angle mobj) angle))))
          (setf (emo-momx mobj) (eldoom-math-mul speed (eldoom-math-cos angle))
                (emo-momy mobj) (eldoom-math-mul speed (eldoom-math-sin angle)))
          (let* ((dist (max 1 (/ (emp-approx-distance (emo-x-fx mobj) (emo-y-fx mobj)
                                                      (emo-x-fx dest) (emo-y-fx dest))
                                 speed)))
                 (slope (/ (- (+ (emo-z-fx dest) (eldoom-math-fixed-from-int 40))
                              (emo-z-fx mobj)) dist)))
            (if (< slope (emo-momz mobj))
                (cl-decf (emo-momz mobj) (/ eldoom-math-unit 8))
              (cl-incf (emo-momz mobj) (/ eldoom-math-unit 8)))))))))

(defun eac--a-skel-whoosh (mobj)
  "A_SkelWhoosh in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eac--face-target mobj target)))

(defun eac--a-skel-fist (mobj)
  "A_SkelFist in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eac--face-target mobj target)
    (when (eac--check-melee-range mobj)
      (let ((damage (eldoom-random-dice 6 10 t)))
        (emo-damage target mobj mobj damage)))))

(defun eac--a-fat-raise (mobj)
  "A_FatRaise in p_enemy.c"
  (eac--a-face-target mobj))

(defconst eac--fat-spread (/ eldoom-math-ang90 8))

(defun eac--a-fat-attack1 (mobj)
  "A_FatAttack1 in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eac--face-target mobj target)
    (emo-spawn-missile mobj target enf-MT_FATSHOT (- eac--fat-spread))
    (emo-spawn-missile mobj target enf-MT_FATSHOT 0)))

(defun eac--a-fat-attack2 (mobj)
  "A_FatAttack2 in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eac--face-target mobj target)
    (emo-spawn-missile mobj target enf-MT_FATSHOT eac--fat-spread)
    (emo-spawn-missile mobj target enf-MT_FATSHOT 0)))

(defun eac--a-fat-attack3 (mobj)
  "A_FatAttack3 in p_enemy.c"
  (when-let* ((target (emo-target mobj)))
    (eac--face-target mobj target)
    (emo-spawn-missile mobj target enf-MT_FATSHOT 0)
    (emo-spawn-missile mobj target enf-MT_FATSHOT (- (/ eac--fat-spread 2)))
    (emo-spawn-missile mobj target enf-MT_FATSHOT (/ eac--fat-spread 2))))


(defvar-local eth--boss-targets nil)
(defvar-local eth--boss-target-count 0)
(defvar-local eth--boss-target-ix 0)
(defun eac--find-boss-targets-callback (thinker _)
  (when (and (emo-p (eth-thinker-object thinker)) 
             (< eth--boss-target-count 32)
             (= (emo-type (eth-thinker-object thinker)) enf-MT_BOSSTARGET))
    (aset eth--boss-targets eth--boss-target-count (eth-thinker-object thinker))
    (cl-incf eth--boss-target-count)))

(defun eac--a-brain-awake (_mobj)
  "A_BrainAwake in p_enemy.c"
  (when (not eth--boss-targets) (setq eth--boss-targets (make-vector 32 nil)))
  (setq eth--boss-target-count 0)
  (eth-for-each-thinker #'eac--find-boss-targets-callback nil)
  (setq eth--boss-target-ix 0))

(defun eac--a-brain-pain (_mobj)
  "A_BrainPain in p_enemy.c")

(defun eac--a-brain-scream (mobj)
  "A_BrainScream in p_enemy.c"
  (cl-loop for x from (- (emo-x-fx mobj) (eldoom-math-fixed-from-int 196))
           below (+ (emo-x-fx mobj) (eldoom-math-fixed-from-int 320))
           by (eldoom-math-fixed-from-int 8) do
           (let* ((y (- (emo-y-fx mobj) (eldoom-math-fixed-from-int 320)))
                  (z (+ 128 (eldoom-math-fixed-from-int (* 2 (eldoom-random t)))))
                  (th (emo-spawn-one x y z enf-MT_ROCKET)))
             (setf (emo-momz th) (* 512 (eldoom-random t)))
             (emo-set-state th (aref enf-states enf-S_BRAINEXPLODE1))
             (setf (emo-tics th) (max (- (emo-tics th) (logand (eldoom-random t) 7)) 1)))))

(defun eac--a-brain-explode (mobj)
  "A_BrainExplode in p_enemy.c"
  (let* ((x (* (- (eldoom-random t) (eldoom-random t)) 2048))
         (y (emo-y-fx mobj))
         (z (+ 128 (eldoom-math-fixed-from-int (* 2 (eldoom-random t)))))
         (th (emo-spawn-one x y z enf-MT_ROCKET)))
    (setf (emo-momz th) (* 512 (eldoom-random t)))
    (emo-set-state th (aref enf-states enf-S_BRAINEXPLODE1))
    (setf (emo-tics th) (max (- (emo-tics th) (logand (eldoom-random t) 7)) 1))))

(defun eac--a-brain-die (_mobj)
  "A_BrainDie in p_enemy.c"
  ;; TODO: exit level
  )

(defun eac--a-boss-death (_mobj)
  "A_BossDeath in p_enemy.c"
  ;; TODO
  )

(defvar-local eac--skip-brain-spit nil)

(defun eac--a-brain-spit (mobj)
  "A_BrainSpit in p_enemy.c"
  (cl-callf not eac--skip-brain-spit)
  (when (or (not (eq eldoom-globals-difficulty :easy))
            (not eac--skip-brain-spit))
    (let* ((target (aref eth--boss-targets eth--boss-target-ix))
           (spawn (emo-spawn-missile mobj target enf-MT_SPAWNSHOT 0)))
      (setf eth--boss-target-ix (mod (1+ eth--boss-target-ix) eth--boss-target-count)
            (emo-target spawn) target
            (emo-reaction-time spawn) (/ (/ (- (emo-y-fx target) (emo-y-fx mobj)) (emo-momy spawn))
                                         (enf-state-tics (emo-state spawn)))))))

(defun eac--a-spawn-fly (mobj)
  "A_SpawnFly in p_enemy.c"
  (cl-decf (emo-reaction-time mobj))
  (when (zerop (emo-reaction-time mobj))
    (let* ((target (emo-target mobj))
           (_ (emo-spawn-one (emo-x-fx target) (emo-y-fx target) (emo-z-fx target) enf-MT_SPAWNFIRE))
           (rand (eldoom-random t))
           (type
            (cond
             ((< rand 50) enf-MT_TROOP)
             ((< rand 90) enf-MT_SERGEANT)
             ((< rand 120) enf-MT_SHADOWS)
             ((< rand 130) enf-MT_PAIN)
             ((< rand 160) enf-MT_HEAD)
             ((< rand 162) enf-MT_PAIN)
             ((< rand 172) enf-MT_UNDEAD)
             ((< rand 192) enf-MT_BABY)
             ((< rand 222) enf-MT_FATSO)
             ((< rand 246) enf-MT_KNIGHT)
             (t enf-MT_BRUISER)))
           (spawn (emo-spawn-one (emo-x-fx target) (emo-y-fx target) (emo-x-fx target) type)))
      (when (eac--look-for-player spawn t)
        (emo-set-state spawn (enf-mobj-info-see-state (emo-info spawn))))
      (eph-teleport-move spawn (emo-x-fx spawn) (emo-y-fx spawn))
      (emo-remove mobj))))

(defun eac--a-spawn-sound (mobj)
  "A_SpawnSound in p_enemy.c"
  (eac--a-spawn-fly mobj))

(defun eac--a-hoof (mobj)
  "A_Hoof in p_enemy.c"
  (eac--a-chase mobj))
(defun eac--a-metal (mobj)
  "A_Metal in p_enemy.c"
  (eac--a-chase mobj))
(defun eac--a-baby-metal (mobj)
  "A_Hoof in p_enemy.c"
  (eac--a-chase mobj))

(defun eac--a-look (mobj)
  "A_Look in p_enemy.c"
  (setf (emo-threshold mobj) 0)
  (let* ((sector (ewd-subsector-sector (emo-subsector mobj)))
         (target (ewd-sector-sound-target sector)))
    (if (and target
             (eldoom-math-has-flag (eldoom-mobj-flags target) enf-MF_SHOOTABLE))
        (progn 
          (setf (eldoom-mobj-target mobj) target)
          (when (or (not (eldoom-math-has-flag (eldoom-mobj-flags mobj) enf-MF_AMBUSH))
                    (eldoom-sight-check mobj target)
                    (eac--look-for-player mobj nil))
            (emo-set-state mobj (enf-mobj-info-see-state (emo-info mobj)))))
      (when (eac--look-for-player mobj nil)
        (emo-set-state mobj (enf-mobj-info-see-state (emo-info mobj)))))))

(defun eac--a-empty (_mobj)
  "A_Pain, A_Scream, A_XScream in p_enemy.c they only emit pain sound"
  )

(defun eac--a-player-scream (_mobj)
  (message "Player scream"))

(defun eac--a-fall (mobj)
  "A_Fall in p_enemy.c"
  (eldoom-math-clear-flagf (emo-flags mobj) enf-MF_SOLID))

(defun eac--a-explode (mobj)
  "A_Explode in p_enemy.c"
  (etr-radius-attack mobj (emo-target mobj) 128))

(defun eac--action-for-name (name)
  (cond
   ((string= name "NULL") nil)
   ((string= name "A_Look") #'eac--a-look)
   ((string= name "A_Chase") #'eac--a-chase)
   ((string= name "A_FaceTarget") #'eac--a-face-target)
   ((string= name "A_PosAttack") #'eac--a-pos-attack)
   ((string= name "A_SPosAttack") #'eac--a-spos-attack)
   ((string= name "A_CPosAttack") #'eac--a-cpos-attack)
   ((string= name "A_CPosRefire") #'eac--a-cpos-refire)
   ((string= name "A_SpidRefire") #'eac--a-spid-refire)
   ((string= name "A_BspiAttack") #'eac--a-bspi-attack)
   ((string= name "A_TroopAttack") #'eac--a-troop-attack)
   ((string= name "A_PainAttack") #'eac--a-pain-attack)
   ((string= name "A_SkullAttack") #'eac--a-skull-attack)
   ((string= name "A_Pain") #'eac--a-empty)
   ((string= name "A_Scream") #'eac--a-empty)
   ((string= name "A_XScream") #'eac--a-empty)
   ((string= name "A_PlayerScream") #'eac--a-player-scream)
   ((string= name "A_Fall") #'eac--a-fall)
   ((string= name "A_VileChase") #'eac--a-vile-chase)
   ((string= name "A_VileStart") #'eac--a-vile-start)
   ((string= name "A_VileTarget") #'eac--a-vile-target)
   ((string= name "A_VileAttack") #'eac--a-vile-attack)
   ((string= name "A_Fire") #'eac--a-fire)
   ((string= name "A_StartFire") #'eac--a-start-fire)
   ((string= name "A_FireCrackle") #'eac--a-fire-crackle)
   ((string= name "A_SargAttack") #'eac--a-sarg-attack)
   ((string= name "A_HeadAttack") #'eac--a-head-attack)
   ((string= name "A_SkelMissile") #'eac--a-skel-missile)
   ((string= name "A_Tracer") #'eac--a-tracer)
   ((string= name "A_SkelWhoosh") #'eac--a-skel-whoosh)
   ((string= name "A_SkelFist") #'eac--a-skel-fist)
   ((string= name "A_FatRaise") #'eac--a-fat-raise)
   ((string= name "A_FatAttack1") #'eac--a-fat-attack1)
   ((string= name "A_FatAttack2") #'eac--a-fat-attack2)
   ((string= name "A_FatAttack3") #'eac--a-fat-attack3)
   ((string= name "A_BrainAwake") #'eac--a-brain-awake)
   ((string= name "A_BrainPain") #'eac--a-brain-pain)
   ((string= name "A_BrainScream") #'eac--a-brain-scream)
   ((string= name "A_BrainExplode") #'eac--a-brain-explode)
   ((string= name "A_BrainDie") #'eac--a-brain-die)
   ((string= name "A_BrainSpit") #'eac--a-brain-spit)
   ((string= name "A_SpawnFly") #'eac--a-spawn-fly)
   ((string= name "A_SpawnSound") #'eac--a-spawn-sound)
   ((string= name "A_Hoof") #'eac--a-hoof)
   ((string= name "A_Metal") #'eac--a-metal)
   ((string= name "A_BabyMetal") #'eac--a-baby-metal)
   ((string= name "A_CyberAttack") #'eac--a-cyber-attack)
   ((string= name "A_BruisAttack") #'eac--a-bruis-attack)
   ((string= name "A_Explode") #'eac--a-explode)
   ((string= name "A_BossDeath") #'eac--a-boss-death)
   (t name)))

(defun eac-init-actions (states)
  (cl-loop for state across states do
           (let* ((action-name (enf-state-action-name state))
                  (action (eac--action-for-name action-name)))
             (when (or (functionp action)
                       (not (functionp (enf-state-action state))))
               (setf (enf-state-action state) action))))
  (setq eac--skip-brain-spit t))

;; Local Variables:
;; read-symbol-shorthands: (("eac-" . "eldoom-actor-") ("eth-" . "eldoom-think-") ("etr-" . "eldoom-trace-") ("emo-" . "eldoom-mobj-") ("enf-" . "eldoom-info-") ("eph-" . "eldoom-phys-") ("ewd-" . "eldoom-wad-") ("emp-" . "eldoom-map-") ("ese-" . "eldoom-sector-"))
;; End:
(provide 'eldoom-actor)
;;; eldoom-actor.el ends here
