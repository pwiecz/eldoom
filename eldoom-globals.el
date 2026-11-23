;;; eldoom-globals.el --- Doom global variables -*- lexical-binding:t -*-

;; Copyright (C) 2026 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;;; Code:

(require 'eldoom-math)

(defconst eldoom-globals-buffer-name "*ELDoom*"
  "Name of the buffer used for ELDoom display and input.")

(defvar-local eldoom-globals-level-time 0)
(defvar-local eldoom-globals-game-tic 0)
(defvar-local eldoom-globals-iteration 1
  "Used to prevent iterating over the same linedef or sector
more than once in one loop") 
(defvar-local eldoom-globals-difficulty :medium)
(defvar-local eldoom-globals-fast-monsters nil)

(defvar-local eldoom-globals-min-move-time-tics 15
  "Emacs does not recognise key up events - prolong key press until the first
repeat event starts, that is about half a second") ;; TODO: maybe allow stopping the movement by quickly moving backwards

(defconst eldoom-globals-melee-range (eldoom-math-fixed-from-int 64))
(defconst eldoom-globals-missile-range (eldoom-math-fixed-from-int (* 32 64)))
(defconst eldoom-globals-skull-speed (eldoom-math-fixed-from-int 20))

(defconst eldoom-globals-max-health 100)

(defconst eldoom-globals-wp_fist 0)
(defconst eldoom-globals-wp_pistol 1)
(defconst eldoom-globals-wp_shotgun 2)
(defconst eldoom-globals-wp_chaingun 3)
(defconst eldoom-globals-wp_missile 4)
(defconst eldoom-globals-wp_plasma 5)
(defconst eldoom-globals-wp_bfg 6)
(defconst eldoom-globals-wp_chainsaw 7)
(defconst eldoom-globals-wp_supershotgun 8)
(defconst eldoom-globals-num-weapons 9)
(defconst eldoom-globals-wp_no-change 10)

(defconst eldoom-globals-am_clip 0)
(defconst eldoom-globals-am_shell 1)
(defconst eldoom-globals-am_cell 2)
(defconst eldoom-globals-am_missile 3)
(defconst eldoom-globals-num-ammo 4)
(defconst eldoom-globals-am_no-ammo 5)

(defconst eldoom-globals-pw_invulnerability 0)
(defconst eldoom-globals-pw_strength 1)
(defconst eldoom-globals-pw_invisibility 2)
(defconst eldoom-globals-pw_ironfeet 3)
(defconst eldoom-globals-pw_allmap 4)
(defconst eldoom-globals-pw_infrared 5)
(defconst eldoom-globals-num-powers 6)

(defconst eldoom-globals-CF_NOCLIP 1)
(defconst eldoom-globals-CF_GODMODE 2)
(defconst eldoom-globals-CF_NOMOMENTUM 4)

(defconst eldoom-globals-tic-rate 35)
(defconst eldoom-globals-invulnerability-tics (* 30 eldoom-globals-tic-rate))
(defconst eldoom-globals-invisibility-tics (* 60 eldoom-globals-tic-rate))
(defconst eldoom-globals-infrared-tics (* 120 eldoom-globals-tic-rate))
(defconst eldoom-globals-iron-feet-tics (* 60 eldoom-globals-tic-rate))

(provide 'eldoom-globals)

;; End:
;;; eldoom-globals.el ends here
