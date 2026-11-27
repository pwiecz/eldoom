;;; eldoom-user.el --- Handling user commands        -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Piotr Wieczorek

;; Author: Piotr Wieczorek <piotr@opabinia>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'eldoom-actor)
(require 'eldoom-info)
(require 'eldoom-math)
(require 'eldoom-mobj)
(require 'eldoom-phys)
(require 'eldoom-random)
(require 'eldoom-sector)
(require 'eldoom-trace)
(require 'eldoom-wad)

(defconst eldoom-user--max-bob #x100000)
(defconst eldoom-user--bfg-cells 40)
(defvar-local eldoom-user-cmd nil)

(cl-defstruct (eldoom-user-command (:constructor eldoom-user-make-command)
                                   (:copier nil))
  (angle-turn 0 :type fixnum)
  (forward-move 0 :type fixnum)
  (side-move 0 :type fixnum)
  (action-button nil :type boolean)
  (attack-button nil :type boolean)
  (weapon-change-button nil))

(defun eldoom-user--calc-height (player)
  "P_CalcHeight in p_user.c"
  (let* ((mobj (emo-player-mobj player)))
    (setf (emo-player-bob player)
          (min (ash (+ (eldoom-math-mul (emo-momx mobj) (emo-momx mobj))
                       (eldoom-math-mul (emo-momy mobj) (emo-momy mobj)))
                    -2)
               eldoom-user--max-bob))
    (if (or (eldoom-math-has-flag (emo-player-cheats player) eldoom-globals-CF_NOMOMENTUM)
            (not (emo-player-on-ground player)))
        (setf (emo-player-view-z player) (+ (emo-z-fx mobj)
                                            (emo-player-view-height player)))
      (let* ((bob-angle (logand (* eldoom-globals-level-time
                                   (/ eldoom-math-fine-angles 20))
                                eldoom-math-fine-mask))
             (bob (eldoom-math-mul (/ (emo-player-bob player) 2)
                                   (aref eldoom-math--finesine bob-angle)))
             (view-height (emo-player-view-height player))
             (delta-view-height (emo-player-delta-view-height player)))
        (when (= (emo-player-state player) enf-PST_LIVE)
          (cl-incf view-height delta-view-height)
          (when (> view-height emo-view-height)
            (setf view-height emo-view-height
                  delta-view-height 0))
          (when (< view-height (/ emo-view-height 2))
            (setf view-height (/ emo-view-height 2)
                  delta-view-height (max delta-view-height 1)))
          (when (/= 0 delta-view-height)
            (cl-incf delta-view-height (/ eldoom-math-unit 4))
            (when (zerop delta-view-height)
              (setf delta-view-height 1)))
          (setf (emo-player-view-height player) view-height
                (emo-player-delta-view-height player) delta-view-height))
        (setf (emo-player-view-z player)
              (min (+ (emo-z-fx mobj) (emo-player-view-height player) bob)
                   (- (emo-ceiling-z mobj) (eldoom-math-fixed-from-int 4))))))))

(defun eldoom-user--thrust (mobj angle move)
  "P_Thrust in p_user.c"
  (cl-incf (emo-momx mobj) (eldoom-math-mul move (eldoom-math-cos angle)))
  (cl-incf (emo-momy mobj) (eldoom-math-mul move (eldoom-math-sin angle))))

(defun eldoom-user--move-player (player)
  "P_MovePlayer in p_user.c"
  (let* ((mobj (emo-player-mobj player))
         (on-ground (<= (emo-z-fx mobj) (emo-floor-z mobj))))
    (setf (emo-player-on-ground player) on-ground)
    (eldoom-math-angle-incf (emo-angle mobj) (eldoom-user-command-angle-turn eldoom-user-cmd))
    (when (and (/= 0 (eldoom-user-command-forward-move eldoom-user-cmd)) on-ground)
      (eldoom-user--thrust mobj (emo-angle mobj) (* 2048 (eldoom-user-command-forward-move eldoom-user-cmd))))

    (when (and (/= 0 (eldoom-user-command-side-move eldoom-user-cmd)) on-ground)
      (eldoom-user--thrust mobj (- (emo-angle mobj) eldoom-math-ang90)
                           (* 2048 (eldoom-user-command-side-move eldoom-user-cmd))))

    (when (and (or (/= 0 (eldoom-user-command-forward-move eldoom-user-cmd))
                   (/= 0 (eldoom-user-command-side-move eldoom-user-cmd)))
               (eq (emo-state mobj) (aref enf-states enf-S_PLAY)))
      (emo-set-state mobj (aref enf-states enf-S_PLAY_RUN1)))))

(defun eldoom-user--use-traverse (intercept mobj)
  "PTR_UseTraverse in p_map.c"
  (let* ((linedef (etr-intercept-item intercept)))
    (if (= 0 (ewd-linedef-special linedef))
        (progn
          ;; Continue checking if the line has opening
          (when (not (ewd-linedef-back-sector linedef))
            (throw :line-used nil))
          (let* ((front (ewd-linedef-front-sector linedef))
                 (front-floor (ewd-sector-floor-height-fx front))
                 (front-ceiling (ewd-sector-ceiling-height-fx front))
                 (back (ewd-linedef-back-sector linedef))
                 (back-floor (ewd-sector-floor-height-fx back))
                 (back-ceiling (ewd-sector-ceiling-height-fx back)))
            (when (<= (min front-ceiling back-ceiling) (max front-floor back-floor))
              (throw :line-used nil))))
      ;; Lines can only be used from front.
      (when (eldoom-math-on-right-side-p (emo-x-fx mobj) (emo-y-fx mobj) (ewd-linedef-segment linedef))
        (ese-use-special-line mobj linedef)
        (throw :line-used t)))))

(defun eldoom-user--use-lines (mobj)
  "Try to use a special linedef in front of MOBJ; return non-nil if used.
P_UseLines in p_map.c"
  (let* ((x-fx (emo-x-fx mobj))
         (y-fx (emo-y-fx mobj))
         (angle-bam (emo-angle mobj))
         (aim-x (+ x-fx (eldoom-math-mul ese--use-range-fx (eldoom-math-cos angle-bam))))
         (aim-y (+ y-fx (eldoom-math-mul ese--use-range-fx (eldoom-math-sin angle-bam)))))
    (catch :line-used
      (etr-path-traverse x-fx y-fx aim-x aim-y etr-pt-addlines #'eldoom-user--use-traverse mobj)
      nil)))

(cl-defstruct (eldoom-user-sprite (:constructor eldoom-user--make-sprite)
                                  (:copier nil))
  state
  (tics 0 :type fixnum)
  (sx 0 :type fixnum)
  (sy 0 :type fixnum))

(defun eldoom-user-set-sprite (player position state)
  "P_SetPsprite in p_pspr.c"
  (let ((sprite (cl-ecase position (:weapon (emo-player-weapon-sprite player))
                          (:flash (emo-player-flash-sprite player)))))
    (catch :break
      (while t
        (setf (eldoom-user-sprite-state sprite) state)
        (when (not state)
          (throw :break t))
        (setf (eldoom-user-sprite-tics sprite) (enf-state-tics state))
        (when-let* ((action (eldoom-info-state-action state)))
          (funcall action player sprite)
          (when (not (eldoom-user-sprite-state sprite))
            (throw :break t)))
        (setq state (enf-state-next (eldoom-user-sprite-state sprite)))
        (when (/= 0 (eldoom-user-sprite-tics sprite))
          (throw :break t))))))

(defun eldoom-user--check-ammo (player)
  "P_CheckAmmo in p_pspr.c"
  (let* ((weapon-info (aref enf-weapon-infos (emo-player-ready-weapon player)))
         (ammo-type (enf-weapon-info-ammo weapon-info))
         (min-count (cond
                     ((= (emo-player-ready-weapon player) eldoom-globals-wp_bfg) eldoom-user--bfg-cells)
                     ((= (emo-player-ready-weapon player) eldoom-globals-wp_supershotgun) 2)
                     (t 1))))
    (if (or (= ammo-type eldoom-globals-am_no-ammo) (>= (aref (emo-player-ammo player) ammo-type) min-count))
        t
      (let ((player-ammo (emo-player-ammo player))
            (weapon-owned (emo-player-weapon-owned player)))
        (cond
         ((and (aref weapon-owned eldoom-globals-wp_plasma)
               (> (aref player-ammo eldoom-globals-am_cell) 0))
          (setf (emo-player-pending-weapon player) eldoom-globals-wp_plasma))
         ((and (aref weapon-owned eldoom-globals-wp_supershotgun)
               (> (aref player-ammo eldoom-globals-am_shell) 2))
          (setf (emo-player-pending-weapon player) eldoom-globals-wp_supershotgun))
         ((and (aref weapon-owned eldoom-globals-wp_chaingun)
               (> (aref player-ammo eldoom-globals-am_clip) 0))
          (setf (emo-player-pending-weapon player) eldoom-globals-wp_chaingun))
         ((and (aref weapon-owned eldoom-globals-wp_shotgun)
               (> (aref player-ammo eldoom-globals-am_shell) 0))
          (setf (emo-player-pending-weapon player) eldoom-globals-wp_shotgun))
         ((> (aref player-ammo eldoom-globals-am_clip) 0)
          (setf (emo-player-pending-weapon player) eldoom-globals-wp_pistol))
         ((aref weapon-owned eldoom-globals-wp_chainsaw)
          (setf (emo-player-pending-weapon player) eldoom-globals-wp_chainsaw))
         ((and (aref weapon-owned eldoom-globals-wp_missile)
               (> (aref player-ammo eldoom-globals-am_missile) 0))
          (setf (emo-player-pending-weapon player) eldoom-globals-wp_missile))
         ((and (aref weapon-owned eldoom-globals-wp_bfg)
               (> (aref player-ammo eldoom-globals-am_cell) 40))
          (setf (emo-player-pending-weapon player) eldoom-globals-wp_bfg))
         (t 
          (setf (emo-player-pending-weapon player) eldoom-globals-wp_fist)))
        (eldoom-user-set-sprite player :weapon (enf-weapon-info-down-state weapon-info))))))

(defun eldoom-user-fire-weapon (player)
  "P_FireWeapon in p_pspr.c"
  (when (eldoom-user--check-ammo player)
    (emo-set-state (emo-player-mobj player) (aref enf-states enf-S_PLAY_ATK1))
    (let ((new-state (enf-weapon-info-attack-state
                      (aref enf-weapon-infos (emo-player-ready-weapon player)))))
      (eldoom-user-set-sprite player :weapon new-state)
      (eldoom-actor-noise-alert (emo-player-mobj player) (emo-player-mobj player)))))

(defun eldoom-user--bob-weapon (player sprite)
  "Part of A_WeaponReady in p_pspr.c"
  (let* ((fine-angle (logand (* 128 eldoom-globals-level-time) eldoom-math-fine-mask))
         (angle (ash fine-angle eldoom-math-angle-from-fine-shift)))
    (setf (eldoom-user-sprite-sx sprite) (+ eldoom-math-unit (eldoom-math-mul (emo-player-bob player)
                                                                              (eldoom-math-cos angle))))
    (cl-callf logand fine-angle (1- (/ eldoom-math-fine-angles 2)))
    (setq angle (ash fine-angle eldoom-math-angle-from-fine-shift))
    (setf (eldoom-user-sprite-sy sprite) (+ eldoom-math-unit (eldoom-math-mul (emo-player-bob player)
                                                                              (eldoom-math-sin angle))))))

(defun eldoom-user--a-weapon-ready (player sprite)
  "A_WeaponReady in p_pspr.c"
  (let ((mobj (emo-player-mobj player)))
    (when (or (eq (emo-state mobj) (aref enf-states enf-S_PLAY_ATK1))
              (eq (emo-state mobj) (aref enf-states enf-S_PLAY_ATK2)))
      (emo-set-state mobj (aref enf-states enf-S_PLAY))))
  (if (or (/= (emo-player-pending-weapon player) eldoom-globals-wp_no-change)
          (zerop (emo-player-health player)))
      (let ((new-state (enf-weapon-info-down-state (aref enf-weapon-infos (emo-player-ready-weapon player)))))
        (eldoom-user-set-sprite player :weapon new-state))
    (if (eldoom-user-command-attack-button eldoom-user-cmd)
        ;; rocket launcher ang bfg do not autofire
        (if (or (not (emo-player-attack-down player))
                (and (/= (emo-player-ready-weapon player) eldoom-globals-wp_missile)
                     (/= (emo-player-ready-weapon player) eldoom-globals-wp_bfg)))
            (progn
              (setf (emo-player-attack-down player) t)
              (eldoom-user-fire-weapon player))
          (eldoom-user--bob-weapon player sprite))
      (setf (emo-player-attack-down player) nil))
    (eldoom-user--bob-weapon player sprite)))

(defun eldoom-user--a-light0 (player _sprite)
  "A_Light0 in p_pspr.c"
  (setf (emo-player-extra-light player) 0))
(defun eldoom-user--a-light1 (player _sprite)
  "A_Light1 in p_pspr.c"
  (setf (emo-player-extra-light player) 1))
(defun eldoom-user--a-light2 (player _sprite)
  "A_Light2 in p_pspr.c"
  (setf (emo-player-extra-light player) 2))

(defconst eldoom-user--weapon-top (eldoom-math-fixed-from-int 32))
(defconst eldoom-user--weapon-bottom (eldoom-math-fixed-from-int 128))

(defun eldoom-user--bring-up-weapon (player)
  "P_BringUpWeapon in p_pspr.c"
  (when (= (emo-player-pending-weapon player) eldoom-globals-wp_no-change)
    (setf (emo-player-pending-weapon player) (emo-player-ready-weapon player)))
  (let ((new-state (enf-weapon-info-up-state (aref enf-weapon-infos (emo-player-pending-weapon player)))))
    (setf (emo-player-pending-weapon player) eldoom-globals-wp_no-change
          (eldoom-user-sprite-sy (emo-player-weapon-sprite player)) eldoom-user--weapon-bottom)
    (eldoom-user-set-sprite player :weapon new-state)))

(defconst eldoom-user--lower-speed (eldoom-math-fixed-from-int 6))

(defun eldoom-user--a-lower (player sprite)
  "A_Lower in p_pspr.c"
  (cl-incf (eldoom-user-sprite-sy sprite) eldoom-user--lower-speed)
  (cond
   ((< (eldoom-user-sprite-sy sprite) eldoom-user--weapon-bottom) nil)
   ((= (emo-player-state player) enf-PST_DEAD)
    (setf (eldoom-user-sprite-sy sprite) eldoom-user--weapon-bottom))
   ((zerop (emo-player-health player)) 
    (eldoom-user-set-sprite player :weapon nil))
   (t
    (setf (emo-player-ready-weapon player) (emo-player-pending-weapon player))
    (eldoom-user--bring-up-weapon player))))

(defconst eldoom-user--raise-speed (eldoom-math-fixed-from-int 6))

(defun eldoom-user--a-raise (player sprite)
  "A_Raise in p_pspr.c"
  (cl-decf (eldoom-user-sprite-sy sprite) eldoom-user--raise-speed)
  (when (>= (eldoom-user-sprite-sy sprite) eldoom-user--weapon-top)
    (setf (eldoom-user-sprite-sy sprite) eldoom-user--weapon-top)
    (eldoom-user-set-sprite player :weapon
                            (enf-weapon-info-ready-state (aref enf-weapon-infos
                                                               (emo-player-ready-weapon player))))))

(defun eldoom-user--a-gun-flash (player _sprite)
  "A_GunFlash in p_pspr.c"
  (emo-set-state (emo-player-mobj player) (aref enf-states enf-S_PLAY_ATK2))
  (let ((weapon-info (aref enf-weapon-infos (emo-player-ready-weapon player))))
    (eldoom-user-set-sprite player :flash (enf-weapon-info-flash-state weapon-info))))

(defun eldoom-user--a-punch (player _sprite)
  "A_Punch in p_pspr.c"
  (let* ((mobj (emo-player-mobj player))
         (damage (eldoom-random-dice 2 10 t))
         (angle (+ (emo-angle mobj) (ash (- (eldoom-random t) (eldoom-random t)) 18)))
         (aim-result (etr-aim-line-attack mobj angle eldoom-globals-melee-range))
         (slope (etr-aim-result-slope aim-result)))
    (when (/= 0 (aref (emo-player-powers player) eldoom-globals-pw_strength))
      (setq damage (* 10 damage)))
    (when-let* ((target (etr-shoot mobj angle eldoom-globals-melee-range slope damage)))
      (setf (emo-angle mobj) (eldoom-math-atan2 (- (emo-y-fx target) (emo-y-fx mobj))
                                                (- (emo-x-fx target) (emo-x-fx mobj)))))))

(defun eldoom-user--a-saw (player _sprite)
  "A_Saw in p_pspr.c"
  (let* ((mobj (emo-player-mobj player))
         (damage (eldoom-random-dice 2 10 t))
         (angle (+ (emo-angle mobj) (ash (- (eldoom-random t) (eldoom-random t)) 18)))
         (aim-result (etr-aim-line-attack mobj angle (1+ eldoom-globals-melee-range)))
         (slope (etr-aim-result-slope aim-result)))
    (etr-shoot mobj angle (1+ eldoom-globals-melee-range) slope damage)
    (when-let* ((target (etr-aim-result-target aim-result)))
      (let* ((new-angle (eldoom-math-atan2 (- (emo-y-fx target) (emo-y-fx mobj))
                                           (- (emo-x-fx target) (emo-x-fx mobj))))
             (delta (eldoom-math-angle-sub new-angle (emo-angle mobj))))
        (if (> delta eldoom-math-ang180)
            (if (< delta (eldoom-math-ang (/ eldoom-math-ang90 -20)))
                (setf (emo-angle mobj) (eldoom-math-angle-add new-angle (/ eldoom-math-ang90 21)))
              (eldoom-math-angle-decf (emo-angle mobj) (/ eldoom-math-ang90 20)))
          (if (> delta (/ eldoom-math-ang90 20))
              (setf (emo-angle mobj) (eldoom-math-angle-sub new-angle (/ eldoom-math-ang90 21)))
            (eldoom-math-angle-incf (emo-angle mobj) (/ eldoom-math-ang90 20)))))
      (eldoom-math-set-flagf (emo-flags mobj) enf-MF_JUSTATTACKED))))

(defconst eldoom-user--autoaim-distance (eldoom-math-fixed-from-int (* 16 64)))

(defun eldoom-user--spawn-player-missile (mobj type)
  (let* ((angle (emo-angle mobj))
         (aim-result (etr-aim-line-attack mobj angle eldoom-user--autoaim-distance)))
    (unless (etr-aim-result-target aim-result)
      (eldoom-math-angle-incf angle (ash 1 26))
      (setq aim-result (etr-aim-line-attack mobj angle eldoom-user--autoaim-distance))
      (unless (etr-aim-result-target aim-result)
        (eldoom-math-angle-decf angle (ash 2 26))
        (setq aim-result (etr-aim-line-attack mobj angle eldoom-user--autoaim-distance)))
      (unless (etr-aim-result-target aim-result)
        (setf angle (emo-angle mobj)
              (etr-aim-result-slope aim-result) 0)))
    (let* ((missile (emo-spawn (emo-x-fx mobj) (emo-y-fx mobj)
                               (+ (emo-z-fx mobj) (eldoom-math-fixed-from-int 32)) type))
           (speed (enf-mobj-info-speed-fx (emo-info missile))))
      (setf (emo-target missile) mobj
            (emo-angle missile) angle
            (emo-momx missile) (eldoom-math-mul speed (eldoom-math-cos angle))
            (emo-momy missile) (eldoom-math-mul speed (eldoom-math-sin angle))
            (emo-momz missile) (eldoom-math-mul speed (etr-aim-result-slope aim-result)))
      (eph-check-missile-spawn missile))))

(defun eldoom-user--a-fire-missile (player _sprite)
  "A_FireMissile in p_pspr.c"
  (let ((ammo-type (enf-weapon-info-ammo (aref enf-weapon-infos (emo-player-ready-weapon player)))))
    (cl-decf (aref (emo-player-ammo player) ammo-type)))
  (eldoom-user--spawn-player-missile (emo-player-mobj player) enf-MT_ROCKET))

(defun eldoom-user--a-fire-bfg (player _sprite)
  "A_FireBFG in p_pspr.c"
  (let ((ammo-type (enf-weapon-info-ammo (aref enf-weapon-infos (emo-player-ready-weapon player)))))
    (cl-decf (aref (emo-player-ammo player) ammo-type)) eldoom-user--bfg-cells)
  (eldoom-user--spawn-player-missile (emo-player-mobj player) enf-MT_BFG))

(defun eldoom-user--a-fire-plasma (player _sprite)
  "A_FirePlasma in p_pspr.c"
  (let* ((weapon-info (aref enf-weapon-infos (emo-player-ready-weapon player)))
         (ammo-type (enf-weapon-info-ammo weapon-info))
         (flash-state-index (enf-weapon-info-flash-state-index weapon-info)))
    (cl-decf (aref (emo-player-ammo player) ammo-type) eldoom-user--bfg-cells)
    (cl-incf flash-state-index (logand (eldoom-random 1) 1))
    (eldoom-user-set-sprite player :flash (aref enf-states flash-state-index))
    (eldoom-user--spawn-player-missile (emo-player-mobj player) enf-MT_PLASMA)))

(defun eldoom-user--bullet-slope (mobj)
  "P_BulletSlope in p_pspr.c"
  (let* ((angle (emo-angle mobj))
         (aim-result (etr-aim-line-attack mobj angle eldoom-user--autoaim-distance)))
    (when (not (etr-aim-result-target aim-result))
      (eldoom-math-angle-incf angle (ash 1 26))
      (setq aim-result (etr-aim-line-attack mobj angle eldoom-user--autoaim-distance))
      (when (not (etr-aim-result-target aim-result))
        (eldoom-math-angle-decf angle (ash 2 26))
        (setq aim-result (etr-aim-line-attack mobj angle eldoom-user--autoaim-distance))))
    (etr-aim-result-slope aim-result)))

(defun eldoom-user--gun-shot (mobj slope accurate)
  "P_GunShot in p_pspr.c"
  (let ((damage (eldoom-random-dice 5 3 t))
        (angle (emo-angle mobj)))
    (unless accurate
      (eldoom-math-angle-incf angle (ash (- (eldoom-random t) (eldoom-random t)) 18)))
    (etr-shoot mobj angle eldoom-globals-missile-range slope damage)))

(defun eldoom-user--a-fire-pistol (player _state)
  "A_FirePistol in p_pspr.c"
  (emo-set-state (emo-player-mobj player) (aref enf-states enf-S_PLAY_ATK2))
  (let ((weapon-info (aref enf-weapon-infos (emo-player-ready-weapon player)))
        (slope (eldoom-user--bullet-slope (emo-player-mobj player))))
    (cl-decf (aref (emo-player-ammo player) (enf-weapon-info-ammo weapon-info)))
    (eldoom-user-set-sprite player :flash (enf-weapon-info-flash-state weapon-info))
    (eldoom-user--gun-shot (emo-player-mobj player) slope (zerop (emo-player-refire player)))))

(defun eldoom-user--a-fire-shotgun (player _state)
  "A_FireShotgun in p_pspr.c"
  (emo-set-state (emo-player-mobj player) (aref enf-states enf-S_PLAY_ATK2))
  (let ((weapon-info (aref enf-weapon-infos (emo-player-ready-weapon player)))
        (slope (eldoom-user--bullet-slope (emo-player-mobj player))))
    (cl-decf (aref (emo-player-ammo player) (enf-weapon-info-ammo weapon-info)))
    (eldoom-user-set-sprite player :flash (enf-weapon-info-flash-state weapon-info))
    (cl-loop repeat 7 do
             (eldoom-user--gun-shot (emo-player-mobj player) slope nil))))

(defun eldoom--a-open-shotgun2 (_player _state)
  "A_OpenShotgun2 in p_pspr.c")

(defun eldoom--a-load-shotgun2 (_player _state)
  "A_LoadShotgun2 in p_pspr.c")

(defun eldoom--a-close-shotgun2 (player state)
  "A_CloseShotgun2 in p_pspr.c"
  (eldoom-user--a-refire player state))

(defun eldoom-user--a-fire-shotgun2 (player _state)
  "A_FireShotgun2 in p_pspr.c"
  (emo-set-state (emo-player-mobj player) (aref enf-states enf-S_PLAY_ATK2))
  (let ((weapon-info (aref enf-weapon-infos (emo-player-ready-weapon player)))
        (slope (eldoom-user--bullet-slope (emo-player-mobj player))))
    (cl-decf (aref (emo-player-ammo player) (enf-weapon-info-ammo weapon-info)) 2)
    (eldoom-user-set-sprite player :flash (enf-weapon-info-flash-state weapon-info))
    (cl-loop repeat 20 do
             (let ((damage (eldoom-random-dice 5 3 t))
                   (angle (emo-angle (emo-player-mobj player))))
               (eldoom-math-angle-incf angle (ash (- (eldoom-random t) (eldoom-random t)) 19))
               (etr-shoot (emo-player-mobj player) angle eldoom-globals-missile-range
                          (+ slope (ash (- (eldoom-random t) (eldoom-random t)) 5))
                          damage)))))

(defun eldoom-user--a-fire-cgun (player state)
  "A_FireCGun in p_pspr.c"
  (let* ((weapon-info (aref enf-weapon-infos (emo-player-ready-weapon player)))
         (slope (eldoom-user--bullet-slope (emo-player-mobj player)))
         (flash-state-index (enf-weapon-info-flash-state-index weapon-info)))
    (when (> (aref (emo-player-ammo player) (enf-weapon-info-ammo weapon-info)) 0)
      (emo-set-state (emo-player-mobj player) (aref enf-states enf-S_PLAY_ATK2))
      (cl-decf (aref (emo-player-ammo player) (enf-weapon-info-ammo weapon-info)))
      (cl-incf flash-state-index
               (cond 
                ((= (enf-state-index (eldoom-user-sprite-state state)) enf-S_CHAIN1) 0)
                ((= (enf-state-index (eldoom-user-sprite-state state)) enf-S_CHAIN2) 1)
                (t (error "Unexpected chaingun state %s" (enf-state-name (eldoom-user-sprite-state state))))))
      (eldoom-user-set-sprite player :flash (aref enf-states flash-state-index))
      (eldoom-user--gun-shot (emo-player-mobj player) slope (zerop (emo-player-refire player))))))

(defun eldoom-user--a-bfg-spray (mobj)
  "A_BFGSpray in p_pspr.c"
  (dotimes (i 40)
    (let ((angle (emo-angle mobj)))
      (eldoom-math-angle-decf angle eldoom-math-ang45)
      (eldoom-math-angle-incf angle (* (/ eldoom-math-ang90 40) i))
      (let ((aim-result (etr-aim-line-attack (emo-target mobj) angle eldoom-user--autoaim-distance)))
        (when-let* ((target (etr-aim-result-target aim-result)))
          (emo-spawn (emo-x-fx target) (emo-y-fx target)
                     (+ (emo-z-fx target) (/ (emo-height-fx target) 2)) enf-MT_EXTRABFG)
          (let ((damage 0))
            (cl-loop repeat 15 do
                     (cl-incf damage (eldoom-random-dice 1 8 t)))
            (emo-damage target (emo-target mobj) (emo-target mobj) damage)))))))

(defun eldoom-user--a-bfg-sound (_player _state)
  "A_BFGSound in p_pspr.c")

(defun eldoom-user--a-refire (player _state)
  "A_ReFire in p_pspr.c"
  (if (and (eldoom-user-command-attack-button eldoom-user-cmd)
           (= (emo-player-pending-weapon player) eldoom-globals-wp_no-change)
           (> (emo-player-health player) 0))
      (progn
        (cl-incf (emo-player-refire player))
        (eldoom-user-fire-weapon player))
    (setf (emo-player-refire player) 0)
    (eldoom-user--check-ammo player)))

(defun eldoom-user--a-check-reload (player _state)
  "A_CheckReload in p_pspr.c"
  (eldoom-user--check-ammo player))

(defun eldoom-user--action-for-name (name)
  (cond
   ((string= name "NULL") nil)
   ((string= name "A_WeaponReady") #'eldoom-user--a-weapon-ready)
   ((string= name "A_Light0") #'eldoom-user--a-light0)
   ((string= name "A_Light1") #'eldoom-user--a-light1)
   ((string= name "A_Light2") #'eldoom-user--a-light2)
   ((string= name "A_Lower") #'eldoom-user--a-lower)
   ((string= name "A_Raise") #'eldoom-user--a-raise)
   ((string= name "A_GunFlash") #'eldoom-user--a-gun-flash)
   ((string= name "A_Punch") #'eldoom-user--a-punch)
   ((string= name "A_Saw") #'eldoom-user--a-saw)
   ((string= name "A_FireMissile") #'eldoom-user--a-fire-missile)
   ((string= name "A_FireBFG") #'eldoom-user--a-fire-bfg)
   ((string= name "A_FirePlasma") #'eldoom-user--a-fire-plasma)
   ((string= name "A_FirePistol") #'eldoom-user--a-fire-pistol)
   ((string= name "A_FireShotgun") #'eldoom-user--a-fire-shotgun)
   ((string= name "A_FireShotgun2") #'eldoom-user--a-fire-shotgun2)
   ((string= name "A_OpenShotgun2") #'eldoom--a-open-shotgun2)
   ((string= name "A_LoadShotgun2") #'eldoom--a-load-shotgun2)
   ((string= name "A_CloseShotgun2") #'eldoom--a-close-shotgun2)
   ((string= name "A_FireCGun") #'eldoom-user--a-fire-cgun)
   ((string= name "A_BFGSpray") #'eldoom-user--a-bfg-spray)
   ((string= name "A_BFGsound") #'eldoom-user--a-bfg-sound)
   ((string= name "A_ReFire") #'eldoom-user--a-refire)
   ((string= name "A_CheckReload") #'eldoom-user--a-check-reload)
   (t name)))

(defun eldoom-user-init (player states)
  (setf eldoom-user-cmd (eldoom-user-make-command)
        (emo-player-weapon-sprite player) (eldoom-user--make-sprite)
        (emo-player-flash-sprite player) (eldoom-user--make-sprite))
  (cl-loop for state across states do
           (let* ((action-name (enf-state-action-name state))
                  (action (eldoom-user--action-for-name action-name)))
             (when (or (functionp action)
                       (not (functionp (enf-state-action state))))
               (setf (enf-state-action state) action))))
  ;; P_SetupPsprites
  (setf (emo-player-pending-weapon player) (emo-player-ready-weapon player))
  (eldoom-user--bring-up-weapon player))

(defun eldoom-user--move-sprite (player position)
  "Part of P_MovePsprites in p_pspr.c"
  (let ((sprite (cl-ecase position (:weapon (emo-player-weapon-sprite player))
                          (:flash (emo-player-flash-sprite player)))))
    (when-let* ((state (eldoom-user-sprite-state sprite)))
      (when (/= -1 (eldoom-user-sprite-tics sprite))
        (cl-decf (eldoom-user-sprite-tics sprite))
        (when (zerop (eldoom-user-sprite-tics sprite))
          (eldoom-user-set-sprite player position (enf-state-next state)))))))

(defun eldoom-user--move-sprites (player)
  "P_MovePsprites in p_pspr.c"
  (eldoom-user--move-sprite player :weapon)
  (eldoom-user--move-sprite player :flash)
  (setf (eldoom-user-sprite-sx (emo-player-flash-sprite player))
        (eldoom-user-sprite-sx (emo-player-weapon-sprite player))
        (eldoom-user-sprite-sy (emo-player-flash-sprite player))
        (eldoom-user-sprite-sy (emo-player-weapon-sprite player))))

(defconst eldoom-user--dead-view-height (eldoom-math-fixed-from-int 6))
(defconst eldoom-user--ang5 (/ eldoom-math-ang90 18))
(defconst eldoom-user--ang355 (eldoom-math-ang (- eldoom-user--ang5)))

(defun eldoom-user--death-think (player)
  "P_DeathThink in p_user.c"
  (eldoom-user--move-sprites player)
  (when (> (emo-player-view-height player) eldoom-user--dead-view-height)
    (cl-decf (emo-player-view-height player) eldoom-math-unit))
  (cl-callf max (emo-player-view-height player) eldoom-user--dead-view-height)
  (setf (emo-player-delta-view-height player) 0
        (emo-player-on-ground player) (<= (emo-z-fx (emo-player-mobj player))
                                          (emo-floor-z (emo-player-mobj player))))
  (eldoom-user--calc-height player)
  (let* ((mobj (emo-player-mobj player))
         (attacker (emo-player-attacker player)))
    (if (and attacker (not (eq attacker mobj)))
        (let* ((angle (eldoom-math-atan2 (- (emo-y-fx attacker) (emo-y-fx mobj))
                                         (- (emo-x-fx attacker) (emo-x-fx mobj))))
               (delta (eldoom-math-angle-sub angle (emo-angle mobj))))
          (cond
           ((or (< delta eldoom-user--ang5) (> delta eldoom-user--ang355))
            (setf (emo-angle mobj) angle)
            (when (/= 0 (emo-player-damage-count player))
              (cl-decf (emo-player-damage-count player))))
           ((< delta eldoom-math-ang180)
            (eldoom-math-angle-incf (emo-angle mobj) eldoom-user--ang5))
           (t 
            (eldoom-math-angle-decf (emo-angle mobj) eldoom-user--ang5))))
      (when (/= 0 (emo-player-damage-count player))
        (cl-decf (emo-player-damage-count player)))))
  ;; TODO: restart on use button
  )

(defun eldoom-user--player-in-special-sector (player)
  "P_PlayerInSpecialSector in p_spec.c"
  (let ((sector (ewd-subsector-sector (emo-subsector (emo-player-mobj player))))
        (mobj (emo-player-mobj player))
        (powers (emo-player-powers player)))
    (when (= (emo-z-fx mobj) (ewd-sector-floor-height-fx sector))
      (cl-ecase (ewd-sector-special sector)
        (5 (when (and (zerop (aref powers eldoom-globals-pw_ironfeet))
                      (zerop (logand eldoom-globals-level-time #x1f)))
             (emo-damage mobj nil nil 10)))
        (7 (when (and (zerop (aref powers eldoom-globals-pw_ironfeet))
                      (zerop (logand eldoom-globals-level-time #x1f)))
             (emo-damage mobj nil nil 5)))
        ((4 16) (when (and (or (zerop (aref powers eldoom-globals-pw_ironfeet))
                               (< (eldoom-random t) 5))
                           (zerop (logand eldoom-globals-level-time #x1f)))
                  (emo-damage mobj nil nil 20)))
        (9 (cl-incf (emo-player-secret-count player))
           (setf (ewd-sector-special sector) 0))
        (11 (eldoom-math-clear-flagf (emo-player-cheats player) eldoom-globals-CF_GODMODE)
            (when (zerop (logand eldoom-globals-level-time #x1f))
              (emo-damage mobj nil nil 20))
            (when (<= (emo-player-health player) 10)
              ;; TODO: exit level
              t))))))

(defconst eldoom-user--inverse-colormap 32)

(defun eldoom-user-update-player (player)
  "P_PlayerThink in p_user.c"
  (let ((mobj (emo-player-mobj player)))
    (if (eldoom-math-has-flag (emo-player-cheats player) eldoom-globals-CF_NOCLIP)
        (eldoom-math-set-flagf (emo-flags mobj) enf-MF_NOCLIP)
      (eldoom-math-clear-flagf (emo-flags mobj) enf-MF_NOCLIP))
    (when (eldoom-math-has-flag (emo-flags mobj) enf-MF_JUSTATTACKED)
      (setf (eldoom-user-command-angle-turn eldoom-user-cmd) 0
            (eldoom-user-command-forward-move eldoom-user-cmd) (/ #xc800 512)
            (eldoom-user-command-side-move eldoom-user-cmd) 0
            (emo-flags mobj) (logand (emo-flags mobj) (lognot enf-MF_JUSTATTACKED))))
    (if (= (emo-player-state player) enf-PST_DEAD)
        (eldoom-user--death-think player)
      (if (/= (emo-reaction-time mobj) 0)
          (cl-decf (emo-reaction-time mobj))
        (eldoom-user--move-player player))
      (eldoom-user--calc-height player)
      (when (/= 0 (ewd-sector-special (ewd-subsector-sector (emo-subsector mobj))))
        (eldoom-user--player-in-special-sector player))
      (when-let* ((weapon (eldoom-user-command-weapon-change-button eldoom-user-cmd)))
        (when (and (= weapon eldoom-globals-wp_fist)
                   (aref (emo-player-weapon-owned player) eldoom-globals-wp_chainsaw)
                   (not (and  (= (emo-player-ready-weapon player) eldoom-globals-wp_chainsaw)
                              (/= 0 (aref (emo-player-powers player) eldoom-globals-pw_strength)))))
          (setq weapon eldoom-globals-wp_chainsaw))
        (when (and (= weapon eldoom-globals-wp_shotgun)
                   (aref (emo-player-weapon-owned player) eldoom-globals-wp_supershotgun)
                   (/= (emo-player-ready-weapon player) eldoom-globals-wp_supershotgun))
          (setq weapon eldoom-globals-wp_supershotgun))
        (when (and (aref (emo-player-weapon-owned player) weapon)
                   (/= weapon (emo-player-ready-weapon player)))
          (setf (emo-player-pending-weapon player) weapon)))
      (when (eldoom-user-command-action-button eldoom-user-cmd)
        (eldoom-user--use-lines mobj))
      (eldoom-user--move-sprites player)
      (let ((powers (emo-player-powers player)))
        (when (/= 0 (aref powers eldoom-globals-pw_strength))
          (cl-incf (aref powers eldoom-globals-pw_strength)))
        (when (/= 0 (aref powers eldoom-globals-pw_invulnerability))
          (cl-decf (aref powers eldoom-globals-pw_invulnerability)))
        (when (/= 0 (aref powers eldoom-globals-pw_invisibility))
          (cl-decf (aref powers eldoom-globals-pw_invulnerability))
          (when (zerop (aref powers eldoom-globals-pw_invisibility))
            (eldoom-math-clear-flagf (emo-flags mobj) enf-MF_SHADOW)))
        (when (/= 0 (aref powers eldoom-globals-pw_infrared))
          (cl-decf (aref powers eldoom-globals-pw_infrared)))
        (when (/= 0 (aref powers eldoom-globals-pw_ironfeet))
          (cl-decf (aref powers eldoom-globals-pw_ironfeet)))
        (when (/= 0 (emo-player-damage-count player))
          (cl-decf (emo-player-damage-count player)))
        (when (/= 0 (emo-player-bonus-count player))
          (cl-decf (emo-player-bonus-count player)))
        (setf (emo-player-fixed-colormap player)
              (let ((colormaps (ewd-map-colormaps ewd-current-map))
                    (invul (aref powers eldoom-globals-pw_invulnerability))
                    (infrared (aref powers eldoom-globals-pw_infrared)))
                (cond
                 ((/= 0 invul)
                  (if (or (> invul 128) (/= 0 (logand invul 8)))
                      (aref colormaps eldoom-user--inverse-colormap)
                    nil))
                 ((/= 0 infrared)
                  (if (or (> infrared 128) (/= 0 (logand invul 8)))
                      (aref colormaps 1) nil))
                 (t nil)))))))
  (setf (eldoom-user-command-angle-turn eldoom-user-cmd) 0
        (eldoom-user-command-forward-move eldoom-user-cmd) 0
        (eldoom-user-command-side-move eldoom-user-cmd) 0
        (eldoom-user-command-action-button eldoom-user-cmd) nil
        (eldoom-user-command-attack-button eldoom-user-cmd) nil
        (eldoom-user-command-weapon-change-button eldoom-user-cmd) nil))

;; Local Variables:
;; read-symbol-shorthands: (("enf-" . "eldoom-info-") ("emo-" . "eldoom-mobj-") ("eph-" . "eldoom-phys-") ("ese-" . "eldoom-sector-") ("etr-" . "eldoom-trace-") ("ewd-" . "eldoom-wad-"))
;; End:
(provide 'eldoom-user)
;;; eldoom-user.el ends here
