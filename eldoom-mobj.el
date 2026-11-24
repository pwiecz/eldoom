;;; eldoom-mobj.el --- Doom map objects and blocklinks -*- lexical-binding:t -*-

;; Copyright (C) 2026 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Runtime map objects (mobj_t) with blockmap thing lists and subsector
;; links, modelled on vanilla P_SetThingPosition / blocklinks.

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'eldoom-globals)
(require 'eldoom-info)
(eval-when-compile (require 'eldoom-intrusive))
(require 'eldoom-map)
(require 'eldoom-math)
(require 'eldoom-random)
(require 'eldoom-wad)

(defconst emo--default-monster-radius-fx (eldoom-math-fixed-from-int 20))
(defconst emo--default-monster-height-fx (eldoom-math-fixed-from-int 56))
(defconst emo-view-height (eldoom-math-fixed-from-int 41))
(defconst emo-float-speed (eldoom-math-fixed-from-int 4))
(defconst emo-gravity (eldoom-math-fixed-from-int 1))

(defvar-local emo-player-mobj nil)

(cl-defstruct (emo-player (:constructor emo--make-player) (:copier nil))
  mobj
  (health 0 :type fixnum)
  (state 0 :type fixnum)
  (on-ground t :type boolean)
  (view-z 0 :type fixnum)
  (view-height 0 :type fixnum)
  (delta-view-height 0 :type fixnum)
  (bob 0 :type fixnum)
  (ready-weapon 0 :type fixnum)
  (pending-weapon 0 :type fixnum)
  (armor-type 0 :type fixnum)
  (armor-points 0 :type fixnum)
  attacker
  (damage-count 0 :type fixnum)
  (kill-count 0 :type fixnum)
  (item-count 0 :type fixnum)
  (bonus-count 0 :type fixnum)
  (secret-count 0 :type fixnum)
  cards
  (backpack nil :type boolean)
  weapon-owned
  max-ammo
  ammo
  message
  weapon-sprite
  flash-sprite
  (attack-down nil :type boolean)
  (extra-light 0 :type fixnum)
  (refire 0 :type fixnum)
  (cheats 0 :type fixnum)
  powers
  fixed-colormap)

(cl-defstruct (eldoom-mobj (:constructor eldoom-mobj--make) (:copier nil))
  (x-fx 0 :type fixnum)
  (y-fx 0 :type fixnum)
  (z-fx 0 :type fixnum)
  (radius-fx emo--default-monster-radius-fx :type fixnum)
  (height-fx emo--default-monster-height-fx :type fixnum)
  (flags 0 :type fixnum)
  (type 0 :type fixnum)
  player
  removed-p
  sector-prev
  sector-next
  block-prev
  block-next
  subsector
  (floor-z 0 :type fixnum)
  (ceiling-z 0 :type fixnum)
  (angle 0 :type fixnum)
  (momx 0 :type fixnum)
  (momy 0 :type fixnum)
  (momz 0 :type fixnum)
  (health 0 :type fixnum)
  (move-direction 0 :type fixnum)
  (move-count 0 :type fixnum)
  target
  tracer
  (reaction-time 0 :type fixnum)
  (threshold 0 :type fixnum)
  shooter
  thinker
  (projectile-p nil)
  state
  (tics 0 :type fixnum)
  frame
  sprite
  info
  on-pick-up
  freelist-next) ;; next mobj in freelist

(define-eldoom-intrusive
 eldoom-mobj-sector eldoom-mobj-sector-prev eldoom-mobj-sector-next)
(define-eldoom-intrusive
 eldoom-mobj-block eldoom-mobj-block-prev eldoom-mobj-block-next)

(defconst emo--multiplier-only 16)

(define-eldoom-intrusive-freelist eldoom-mobj-
                                  eldoom-mobj-freelist-next
                                  eldoom-mobj--make)

(defun emo--new-mobj ()
  (let ((mobj (emo--new-from-freelist)))
    (setf (emo-x-fx mobj) 0
          (emo-y-fx mobj) 0
          (emo-z-fx mobj) 0
          (emo-radius-fx mobj) emo--default-monster-radius-fx
          (emo-height-fx mobj) emo--default-monster-height-fx
          (emo-flags mobj) 0
          (emo-type mobj) 0
          (emo-player mobj) nil
          (emo-removed-p mobj) nil
          (emo-sector-next mobj) nil
          (emo-sector-prev mobj) nil
          (emo-block-next mobj) nil
          (emo-block-prev mobj) nil
          (emo-subsector mobj) nil
          (emo-floor-z mobj) 0
          (emo-ceiling-z mobj) 0
          (emo-angle mobj) 0
          (emo-momx mobj) 0
          (emo-momy mobj) 0
          (emo-momz mobj) 0
          (emo-move-direction mobj) 0
          (emo-move-count mobj) 0
          (emo-target mobj) nil
          (emo-tracer mobj) nil
          (emo-reaction-time mobj) 0
          (emo-threshold mobj) 0
          (emo-shooter mobj) nil
          (emo-thinker mobj) nil
          (emo-projectile-p mobj) nil
          (emo-state mobj) nil
          (emo-tics mobj) 0
          (emo-frame mobj) nil
          (emo-sprite mobj) nil
          (emo-info mobj) nil
          (emo-freelist-next mobj) nil)
    mobj))

(defvar-local emo--spawn-functions nil)

(defun emo-add-spawn-function (func)
  (setq emo--spawn-functions (cons func emo--spawn-functions)))

(defun emo-spawn-one (x-fx y-fx z info-index)
  "P_SpawnMapThing in p_mobj.c"
  (let* ((info (aref enf-mobj-infos info-index))
         (spawn-state (enf-mobj-info-spawn-state info))
         (mobj (emo--new-mobj)))
    (setf (emo-x-fx mobj) x-fx
          (emo-y-fx mobj) y-fx
          (emo-reaction-time mobj) (if (eq eldoom-globals-difficulty :nightmare)
                                       0
                                     (enf-mobj-info-reaction-time info))
          (emo-threshold mobj) 0
          (emo-tics mobj) 0
          (emo-radius-fx mobj) (enf-mobj-info-radius-fx info)
          (emo-height-fx mobj) (enf-mobj-info-height-fx info)
          (emo-flags mobj) (enf-mobj-info-flags info)
          (emo-type mobj) info-index
          (emo-health mobj) (enf-mobj-info-spawn-health info)
          (emo-state mobj) spawn-state
          (emo-tics mobj) (enf-state-tics spawn-state)
          (emo-sprite mobj) (enf-state-sprite spawn-state)
          (emo-frame mobj) (enf-state-frame spawn-state)
          (emo-info mobj) info)
    (emo-set-position mobj)
    (when-let* ((subsector (emo-subsector mobj))
                (sector (ewd-subsector-sector subsector)))
      (setf (emo-floor-z mobj) (ewd-sector-floor-height-fx sector)
            (emo-ceiling-z mobj) (ewd-sector-ceiling-height-fx sector)))
    (setf (emo-z-fx mobj)
          (cond
           ((fixnump z) z)
           ((eq z :on-floor) (emo-floor-z mobj))
           ((eq z :on-ceiling) (- (emo-ceiling-z mobj) (emo-height-fx mobj)))
           (t (error "Unexpected mobj z coordinate %S" z))))
    (dolist (func emo--spawn-functions)
      (funcall func mobj))
    mobj))

(defconst emo--max-health 100)

(defun emo-spawn-player (thing)
  "Create the player mobj (not tied to a map thing).
Almost P_SpawnPlayer in p_mobj.c"
  (let* ((x (ewd-thing-x-fx thing))
         (y (ewd-thing-y-fx thing))
         (z :on-floor)
         (mobj (emo-spawn-one x y z enf-MT_PLAYER))
         (player (emo--make-player :mobj mobj
                                   :state enf-PST_LIVE
                                   :view-height emo-view-height
                                   :health emo--max-health
                                   :weapon-owned (make-bool-vector eldoom-globals-num-weapons nil)
                                   :ready-weapon eldoom-globals-wp_pistol
                                   :max-ammo (vector 200 50 300 50)
                                   :ammo (vector 50 0 0 0)
                                   :powers (make-vector eldoom-globals-num-powers 0))))
    (setf (emo-angle mobj) (* eldoom-math-ang45 (/ (ewd-thing-angle-deg thing) 45))
          (emo-player mobj) player
          (emo-health mobj) (emo-player-health player)
          emo-player-mobj mobj)
    (aset (emo-player-weapon-owned player) eldoom-globals-wp_pistol t)
    player))

(defun emo-spawn-missile (source dest type angle-delta)
  "P_SpawnMissile in p_mobj.c"
  (let* ((source-x (emo-x-fx source))
         (source-y (emo-y-fx source))
         (source-z (emo-z-fx source))
         (dest-x (emo-x-fx dest))
         (dest-y (emo-y-fx dest))
         (dest-z (emo-z-fx dest))
         (missile (emo-spawn-one source-x source-y 
                                 (+ source-z (eldoom-math-fixed-from-int 32))
                                 type))
         (speed (enf-mobj-info-speed-fx (emo-info missile)))
         (angle (eldoom-math-angle-add (eldoom-math-atan2 (- dest-y source-y)
                                                          (- dest-x source-x))
                                       angle-delta))
         (time-to-dest (/ (emp-approx-distance source-x source-y dest-x dest-y) speed)))
    (when (eldoom-math-has-flag (emo-flags dest) enf-MF_SHADOW)
      (eldoom-math-angle-incf angle (ash (- (eldoom-random t) (eldoom-random t)) 20)))
    (setf (emo-target missile) source
          (emo-angle missile) angle
          (emo-momx missile) (eldoom-math-mul speed (eldoom-math-cos angle))
          (emo-momy missile) (eldoom-math-mul speed (eldoom-math-sin angle))
          (emo-momz missile) (/ (- dest-z source-z) (max time-to-dest 1)))
    missile))

(defun emo-explode-missile (missile)
  "P_ExplodeMissile in p_mobj.c"
  (setf (emo-momx missile) 0
        (emo-momy missile) 0
        (emo-momz missile) 0)
  (emo-set-state missile (enf-mobj-info-death-state (emo-info missile)))
  (setf (emo-tics missile) (max (- (emo-tics missile) (logand (eldoom-random t) 3)) 1)
        (emo-flags missile) (eldoom-math-clear-flag (emo-flags missile) enf-MF_MISSILE)))

(defun emo-spawn-puff (x y z punch-p)
  "P_SpawnPuff in p_mobj.c"
  (cl-incf z (ash (- (eldoom-random t) (eldoom-random t)) 10))
  (let ((mobj (emo-spawn-one x y z enf-MT_PUFF)))
    (setf (emo-momz mobj) (eldoom-math-fixed-from-int 2)
          (emo-tics mobj) (max (- (emo-tics mobj) (logand (eldoom-random t) 3)) 1))
    ;; don't make punches spark on the wall
    (when punch-p
      (emo-set-state mobj (aref enf-states enf-S_PUFF3)))))

(defun emo-spawn-blood (x y z damage)
  "P_SpawnBlood in p_mobj.c"
  (cl-incf z (ash (- (eldoom-random t) (eldoom-random t)) 10))
  (let ((mobj (emo-spawn-one x y z enf-MT_BLOOD)))
    (setf (emo-momz mobj) (eldoom-math-fixed-from-int 2)
          (emo-tics mobj) (max (- (emo-tics mobj) (logand (eldoom-random t) 3)) 1))
    (cond
     ((<= 9 damage 12) (emo-set-state mobj (aref enf-states enf-S_BLOOD2)))
     ((< damage 9) (emo-set-state mobj (aref enf-states enf-S_BLOOD3))))))

(defsubst emo--difficulty-to-bit (difficulty)
  (cl-ecase difficulty
    ((:baby :easy) 1)
    (:medium 2)
    ((:hard :nightmare) 4)))

(defun emo-init-things (map)
  "P_LoadThings in p_setup.c (more or less)
Spawn mobj_t for all collidable things on MAP. Fills mobj blockmap in map."
  (let* ((blockmap (ewd-map-blockmap map))
         (blockmap-length (length (ewd-blockmap-lines-blockmap blockmap)))
         (mobj-blockmap (make-vector blockmap-length nil))
         (difficulty-bit (emo--difficulty-to-bit eldoom-globals-difficulty)))
    (dotimes (i blockmap-length)
      (aset mobj-blockmap i (emo--make)))
    (setf (ewd-blockmap-mobj-blockmap blockmap) mobj-blockmap)
    (cl-loop for thing across (ewd-map-things map) do
             (let ((flags (ewd-thing-flags thing)))
               (when (and (not (eldoom-math-has-flag flags emo--multiplier-only))
                          (eldoom-math-has-flag flags difficulty-bit))
                 (let* ((x (ewd-thing-x-fx thing))
                        (y (ewd-thing-y-fx thing))
                        (type (ewd-thing-type thing))
                        (index (enf-index-for-type enf-mobj-infos type)))
                   (when (and (> type 4) (/= type 11)) ;; don't spawn players or deathmatch starts here
                     (cl-assert index)
                     (let* ((info (aref enf-mobj-infos index))
                            (z (if (eldoom-math-has-flag (enf-mobj-info-flags info) enf-MF_SPAWNCEILING)
                                   :on-ceiling :on-floor))
                            (mobj (emo-spawn-one x y z index)))
                       (setf (emo-angle mobj) (* eldoom-math-ang45 (/ (ewd-thing-angle-deg thing) 45)))
                       (when (eldoom-math-has-flag (ewd-thing-flags thing) enf-MTF_AMBUSH)
                         (eldoom-math-set-flagf (emo-flags mobj) enf-MF_AMBUSH))))))))))

(defun emo--block-index (blockmap x-fx y-fx)
  (let* ((column-count (ewd-blockmap-column-count blockmap))
         (row-count (ewd-blockmap-row-count blockmap))
         (origin-x-fx (ewd-blockmap-origin-x-fx blockmap))
         (origin-y-fx (ewd-blockmap-origin-y-fx blockmap))
         (bx (ash (- x-fx origin-x-fx) ewd-map-block-shift))
         (by (ash (- y-fx origin-y-fx) ewd-map-block-shift)))
    (when (and (>= bx 0) (< bx column-count) (>= by 0) (< by row-count))
      (+ bx (* by column-count)))))

(defun emo--unlink-block (mobj)
  "Remove MOBJ from the blockmap"
  (emo-block-remove mobj))

(defun emo--link-block (mobj)
  "Add MOBJ to the blockmap"
  (let* ((blockmap (ewd-map-blockmap ewd-current-map))
         (idx (emo--block-index blockmap (emo-x-fx mobj) (emo-y-fx mobj)))
         (mobj-blockmap (ewd-blockmap-mobj-blockmap blockmap)))
    (when idx
      (emo-block-insert-after (aref mobj-blockmap idx) mobj))))

(defun emo--unlink-sector (mobj)
  (emo-sector-remove mobj)
  (setf (emo-subsector mobj) nil))

(defun emo--link-sector (mobj)
  (when-let* ((ss (emp-subsector-at (emo-x-fx mobj) (emo-y-fx mobj)))
              (s (ewd-subsector-sector ss)))
    (when (not (ewd-sector-thing s))
      (setf (ewd-sector-thing s) (emo--make)))
    (setf (emo-subsector mobj) ss)
    (let ((head (ewd-sector-thing s)))
      (emo-sector-insert-after head mobj))))

(defun emo-unset-position (mobj)
  "Remove MOBJ from blockmap and subsector lists."
  (emo--unlink-block mobj)
  (emo--unlink-sector mobj))

(defun emo-set-position (mobj)
  "P_SetThingPosition in p_maputl.c
Place MOBJ in blockmap and sector lists."
  (emo-unset-position mobj)
  (unless (emo-removed-p mobj)
    (setf (emo-subsector mobj) (emp-subsector-at (emo-x-fx mobj) (emo-y-fx mobj)))
    (unless (eldoom-math-has-flag (emo-flags mobj) enf-MF_NOBLOCKMAP)
      (emo--link-block mobj))
    (unless (eldoom-math-has-flag (emo-flags mobj) enf-MF_NOSECTOR)
      (emo--link-sector mobj))))

(defun emo-monster-p (mobj)
  "Return non-nil if MOBJ is a shootable non-player monster."
  (and mobj
       (not (emo-player mobj))
       (not (emo-projectile-p mobj))
       (not (emo-removed-p mobj))
       (eldoom-math-has-flag (emo-flags mobj) enf-MF_SHOOTABLE)))

(defvar-local emo--remove-functions nil)

(defun emo-add-remove-function (func)
  (setq emo--remove-functions (cons func emo--remove-functions)))

(defun emo-remove (mobj)
  "Mark MOBJ removed and take it out of spatial indexes."
  (setf (emo-removed-p mobj) t)
  (dolist (func emo--remove-functions)
    (funcall func mobj))
  (emo-unset-position mobj)
  (emo--move-to-freelist mobj))

(defun emo-set-state (mobj state)
  "Returns not nil if mobj is still present (not removed)
P_SetMobjState in p_mobj.c"
  (not
   (catch :removed
     (while t
       (when (not state)
         (setf (emo-state mobj) nil)
         (emo-remove mobj)
         (throw :removed t))
       (setf (emo-state mobj) state
             (emo-tics mobj) (enf-state-tics state)
             (emo-sprite mobj) (enf-state-sprite state)
             (emo-frame mobj) (enf-state-frame state))
       (when-let* ((action (enf-state-action state)))
         (funcall action mobj))
       (setq state (enf-state-next state))
       (when (not (zerop (emo-tics mobj)))
         (throw :removed nil))))))

(defun emo-for-each-in-block (bx by fn obj)
  "Call FN with each mobj in given blockmap block
passing OBJ as the second argument.
P_BlockThingsiterator in p_maputl.c"
  (let ((blockmap (ewd-map-blockmap ewd-current-map)))
    (when (and (<= 0 bx)
               (< bx (ewd-blockmap-column-count blockmap))
               (<= 0 by)
               (< by (ewd-blockmap-row-count blockmap)))
      (let* ((blockmap-index (emp-blockmap-index blockmap bx by))
             (mobj-blockmap (ewd-blockmap-mobj-blockmap blockmap))
             (mobj (emo-block-next (aref mobj-blockmap blockmap-index))))
        (while mobj
          (funcall fn mobj obj)
          (setq mobj (emo-block-next mobj)))))))

(defun emo-for-each-in-block-overlapping-bbox (left right bottom top fn obj)
  "Call FN with each mobj in a blockmap block overlapping given bbox,
 passing OBJ as the second argument."
  (let* ((blockmap (ewd-map-blockmap ewd-current-map))
         (origin-x (ewd-blockmap-origin-x-fx blockmap))
         (origin-y (ewd-blockmap-origin-y-fx blockmap))
         (bl (ash (- left origin-x ewd-max-radius) ewd-map-block-shift))
         (br (ash (+ (- right origin-x) ewd-max-radius) ewd-map-block-shift))
         (bb (ash (- bottom origin-y ewd-max-radius) ewd-map-block-shift))
         (bt (ash (+ (- top origin-y) ewd-max-radius) ewd-map-block-shift)))
    (cl-loop for bx from bl to br do
             (cl-loop for by from bb to bt do
                      (emo-for-each-in-block bx by fn obj)))))

(defun emo--kill (target)
  "P_KillMobj in p_inter.c"
  (eldoom-math-clear-flagf (emo-flags target)
                           (logior enf-MF_SHOOTABLE enf-MF_FLOAT enf-MF_SKULLFLY))
  (when (/= (emo-type target) enf-MT_SKULL)
    (eldoom-math-clear-flag (emo-flags target) enf-MF_NOGRAVITY))
  (setf (emo-flags target) (logior (emo-flags target) enf-MF_CORPSE enf-MF_DROPOFF)
        (emo-height-fx target) (/ (emo-height-fx target) 4))
  (when (eldoom-math-has-flag (emo-flags target) enf-MF_COUNTKILL)
    (cl-incf (emo-player-kill-count (emo-player emo-player-mobj))))
  (when-let* ((player (emo-player target)))
    (eldoom-math-clear-flag (emo-flags target) enf-MF_SOLID)
    (setf (emo-player-state player) enf-PST_DEAD)
    ;; TODO: exit automap and such
    )
  (if (and (< (emo-health target) (- (enf-mobj-info-spawn-health (emo-info target))))
           (enf-mobj-info-x-death-state (emo-info target)))
      (emo-set-state target (enf-mobj-info-x-death-state (emo-info target)))
    (emo-set-state target (enf-mobj-info-death-state (emo-info target))))
  (setf (emo-tics target) (max (- (emo-tics target) (logand (eldoom-random t) 3)) 1))
  (when-let* ((type (emo-type target))
              (item
               (cond
                ((or (= type enf-MT_WOLFSS) (= type enf-MT_POSSESSED)) enf-MT_CLIP)
                ((= type enf-MT_SHOTGUY) enf-MT_SHOTGUN)
                ((= type enf-MT_CHAINGUY) enf-MT_CHAINGUN))))
    (let ((drop (emo-spawn-one (emo-x-fx target) (emo-y-fx target) :on-floor item)))
      (eldoom-math-set-flagf (emo-flags drop) enf-MF_DROPPED))))

(defconst emo--base-threshold 100)

(defun emo-damage (target inflictor source damage)
  "P_DamageMobj in p_inter.c"
  (message "Damage %d" damage)
  (catch :return
    (when (and (eldoom-math-has-flag (emo-flags target) enf-MF_SHOOTABLE)
               (> (emo-health target) 0))
      (when (eldoom-math-has-flag (emo-flags target) enf-MF_SKULLFLY)
        (setf (emo-momx target) 0
              (emo-momy target) 0
              (emo-momz target) 0))
      (when (and (emo-player target)
                 (eq eldoom-globals-difficulty :baby))
        (setq damage (/ damage 2)))
      ;; kick away unless using the chainsaw
      (when (and inflictor
                 (not (eldoom-math-has-flag (emo-flags target) enf-MF_NOCLIP))
                 (or (not source)
                     (not (emo-player source))
                     (not (eq (emo-player-ready-weapon (emo-player source)) 
                              eldoom-globals-wp_chainsaw))))
        (let ((angle (eldoom-math-atan2 (- (emo-y-fx target) (emo-y-fx inflictor))
                                        (- (emo-x-fx target) (emo-x-fx inflictor))))
              (thrust (/ (* damage (/ eldoom-math-unit 8) 100) (enf-mobj-info-mass (emo-info target)))))
          ;; make fall forwards sometimes
          (when (and (< damage 40)
                     (> damage (emo-health target))
                     (> (- (emo-z-fx target) (emo-z-fx inflictor)) (eldoom-math-fixed-from-int 64))
                     (not (zerop (logand (eldoom-random t) 1))))
            (cl-incf angle eldoom-math-ang45)
            (setq thrust (* thrust 4)))
          (cl-incf (emo-momx target) (eldoom-math-mul thrust (eldoom-math-cos angle)))
          (cl-incf (emo-momy target) (eldoom-math-mul thrust (eldoom-math-sin angle)))))
      (when-let* ((player (emo-player target)))
        ;; end of game hell hack
        (when (and (eq (ewd-sector-special (ewd-subsector-sector (emo-subsector target))) 11)
                   (>= damage (emo-health target)))
          (setq damage (1- (emo-health target))))
        (when (and (< damage 1000)
                   (or (/= 0 (logand (emo-player-cheats player) eldoom-globals-CF_GODMODE))
                       (/= 0 (aref (emo-player-powers player) eldoom-globals-pw_invulnerability))))
          (throw :return t))
        (when (/= 0 (emo-player-armor-type player))
          (let ((saved (if (= (emo-player-armor-type player) 1) (/ damage 3) (/ damage 2))))
            (when (<= (emo-player-armor-points player) saved)
              ;; armor is used up
              (setf saved (emo-player-armor-points player)
                    (emo-player-armor-type player) 0))
            (cl-decf (emo-player-armor-points player) saved)
            (cl-decf damage saved)))
        (setf (emo-player-health player) (max (- (emo-player-health player) damage) 0)
              (emo-player-attacker player) source
              (emo-player-damage-count player) (min (+ (emo-player-damage-count player) damage) 100)))
      (cl-decf (emo-health target) damage)
      (if (<= (emo-health target) 0)
          (emo--kill target)
        (when (and (< (eldoom-random t) (enf-mobj-info-pain-chance (emo-info target)))
                   (not (eldoom-math-has-flag (emo-flags target) enf-MF_SKULLFLY)))
          (eldoom-math-set-flagf (emo-flags target) enf-MF_JUSTHIT)
          (emo-set-state target (enf-mobj-info-pain-state (emo-info target))))
        (setf (emo-reaction-time target) 0)
        (when (and (or (/= 0 (emo-threshold target)) (/= (emo-type target) enf-MT_VILE))
                   source
                   (not (eql source target))
                   (/= (emo-type source) enf-MT_VILE))
          ;; if not intent on another player chase after this one
          (setf (emo-target target) source
                (emo-threshold target) emo--base-threshold)
          (when (and (eql (emo-state target) (enf-mobj-info-spawn-state (emo-info target)))
                     (enf-mobj-info-see-state (emo-info target)))
            (emo-set-state target (enf-mobj-info-see-state (emo-info target)))))))))

(defconst emo--bonus-add 6)

(defconst emo--clip-ammo (vector 10 4 20 1))

(defun emo--give-ammo (player ammo num)
  "P_GiveAmmo in p_inter.c"
  (when (< ammo eldoom-globals-am_no-ammo)
    (cl-assert (<= 0 ammo eldoom-globals-am_no-ammo))
    (when (< (aref (emo-player-ammo player) ammo) (aref (emo-player-max-ammo player) ammo))
      (if (zerop num)
          (setq num (/ (aref emo--clip-ammo ammo) 2))
        (setq num (* num (aref emo--clip-ammo ammo))))
      (when (or (eq eldoom-globals-difficulty :baby)
                (eq eldoom-globals-difficulty :nightmare))
        (setq num (* 2 num)))
      (let ((old-ammo (aref (emo-player-ammo player) ammo)))
        (setf (aref (emo-player-ammo player) ammo) (min (+ num old-ammo)
                                                        (aref (emo-player-max-ammo player) ammo)))
        (when (zerop old-ammo)
          ;; we were down to zero, select a new weapon
          (let ((ready-weapon (emo-player-ready-weapon player)))
            (cond
             ((= ammo eldoom-globals-am_clip)
              (when (= ready-weapon eldoom-globals-wp_fist)
                (setf (emo-player-pending-weapon player) 
                      (if (aref (emo-player-weapon-owned player) eldoom-globals-wp_chaingun)
                          eldoom-globals-wp_chaingun
                        eldoom-globals-wp_pistol))))
             ((= ammo eldoom-globals-am_shell)
              (when (and (or (= ready-weapon eldoom-globals-wp_fist)
                             (= ready-weapon eldoom-globals-wp_pistol))
                         (aref (emo-player-weapon-owned player) eldoom-globals-wp_shotgun))
                (setf (emo-player-pending-weapon player) eldoom-globals-wp_shotgun)))
             ((= ammo eldoom-globals-am_cell)
              (when (and (or (= ready-weapon eldoom-globals-wp_fist)
                             (= ready-weapon eldoom-globals-wp_pistol))
                         (aref (emo-player-weapon-owned player) eldoom-globals-wp_plasma))
                (setf (emo-player-pending-weapon player) eldoom-globals-wp_plasma)))
             ((= ammo eldoom-globals-am_missile)
              (when (and (= ready-weapon eldoom-globals-wp_fist)
                         (aref (emo-player-weapon-owned player) eldoom-globals-wp_missile))
                (setf (emo-player-pending-weapon player) eldoom-globals-wp_missile)))))))
      t)))

(defun emo--give-weapon (player weapon dropped)
  "P_GiveWeapon in p_inter.c"
  (let* ((ammo (enf-weapon-info-ammo (aref enf-weapon-infos weapon)))
         (gave-ammo (when (/= ammo eldoom-globals-am_no-ammo)
                      (emo--give-ammo player ammo (if dropped 1 2)))))
    (if (aref (emo-player-weapon-owned player) weapon)
        gave-ammo
      (aset (emo-player-weapon-owned player) weapon t)
      (setf (emo-player-pending-weapon player) weapon)
      t)))

(defun emo--give-body (player num)
  "P_GiveBody in p_inter.c"
  (when (< (emo-player-health player) eldoom-globals-max-health)
    (setf (emo-player-health player) (min (+ (emo-player-health player) num)
                                          eldoom-globals-max-health)
          (emo-health (emo-player-mobj player)) (emo-player-health player))
    t))

(defun emo--give-armor (player armor-type)
  "P_GiveArmor in p_inter.c"
  (let ((hits (* 100 armor-type)))
    (when (< (emo-player-armor-points player) hits)
      (setf (emo-player-armor-type player) armor-type
            (emo-player-armor-points player) hits)
      t)))

(defconst emo--bonus-add 6)
(defun emo--give-card (player card)
  "P_GiveCard in p_inter.c"
  (when (not (memq card (emo-player-cards player)))
    (setf (emo-player-bonus-count player) emo--bonus-add)
    (push (emo-player-cards player) card)))

(defun emo--give-power (player power)
  "P_GivePower in p_inter.c"
  (let ((powers (emo-player-powers player)))
    (cond
     ((= power eldoom-globals-pw_invulnerability)
      (aset powers power eldoom-globals-invisibility-tics)
      t)
     ((= power eldoom-globals-pw_invisibility)
      (aset powers power eldoom-globals-invisibility-tics)
      (eldoom-math-set-flagf (emo-flags (emo-player-mobj player)) enf-MF_SHADOW)
      t)
     ((= power eldoom-globals-pw_infrared)
      (aset powers power eldoom-globals-infrared-tics)
      t)
     ((= power eldoom-globals-pw_ironfeet)
      (aset powers power eldoom-globals-iron-feet-tics)
      t)
     ((= power eldoom-globals-pw_strength)
      (emo--give-body player 100)
      (aset powers power 1)
      t)
     (t
      (when (zerop (aref powers power))
        (aset powers power 1)
        t)))))

(defun emo-touch-special (special toucher)
  "P_TouchSpecialThing in p_inter.c"
  (let ((delta (- (emo-z-fx special) (emo-z-fx toucher))))
    (when (and (<= delta (emo-height-fx toucher))
               (>= delta (eldoom-math-fixed-from-int -8))
               (> (emo-health toucher) 0))
      (let ((player (emo-player toucher))
            (on-pick-up (emo-on-pick-up special)))
        (when (and on-pick-up (funcall on-pick-up player special))
          (when (eldoom-math-has-flag (emo-flags special) enf-MF_COUNTITEM)
            (cl-incf (emo-player-item-count player)))
          (cl-incf (emo-player-bonus-count player) emo--bonus-add)
          (emo-remove special))))))

;; Local Variables:
;; read-symbol-shorthands: (("emo-" . "eldoom-mobj-") ("enf-" . "eldoom-info-") ("emp-" . "eldoom-map-") ("ewd-" . "eldoom-wad-") ("ewd-" . "eldoom-wad-"))
;; End:
(provide 'eldoom-mobj)
;;; eldoom-mobj.el ends here
