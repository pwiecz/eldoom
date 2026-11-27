;;; eldoom-special.el --- Handlers for linedef actions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Piotr Wieczorek

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Keywords: games
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

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

;; Handlers for linedefs types (switches, walk-over lines, shootable switches)

;;; Code:

(require 'eldoom-info)
(require 'eldoom-math)
(require 'eldoom-mobj)
(require 'eldoom-sector)
(require 'eldoom-wad)

;; All switch handlers must return non-nil is given mobj can use them
;; and nil otherwise.
(defun eldoom-special--door-monster-can-use (mobj linedef)
  "Linedef types 1 32 33 34"
  (when (or (emo-player mobj)
            (not (eldoom-math-has-flag (eldoom-wad-linedef-flags linedef) enf-ML_SECRET)))
    (ese-vertical-door linedef mobj)
    t))
(defun eldoom-special--door (mobj linedef)
  "Linedef types 26 27 28 31 117 118"
  (when (emo-player mobj)
    (ese-vertical-door linedef mobj)
    t))
(defun eldoom-special--s1-build-stairs-8-up (mobj linedef)
  "Linedef type 7"
  (when (emo-player mobj)
    (when (ese-build-stairs linedef :build-8)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-floor-donut (mobj linedef)
  "Linedef type 9"
  (when (emo-player mobj)
    (when (ese-do-donut linedef)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-exit-normal (mobj linedef)
  "Linedef type 11"
  (when (emo-player mobj)
    (ese-change-switch-texture linedef nil)
    ;; TODO exit level
    ))
(defun eldoom-special--s1-floor-up-32-change-texture (mobj linedef)
  "Linedef type 14"
  (when (emo-player mobj)
    (when (ese-do-plat linedef :raise-and-change 32)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-floor-up-24-change-texture (mobj linedef)
  "Linedef type 15"
  (when (emo-player mobj)
    (when (ese-do-plat linedef :raise-and-change 24)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-floor-to-higher-adjacent-floor (mobj linedef)
  "Linedef type 18"
  (when (emo-player mobj)
    (when (ese-do-floor linedef :raise-floor-to-nearest)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-floor-to-higher-floor-change-texture (mobj linedef)
  "Linedef type 20"
  (when (emo-player mobj)
    (when (ese-do-plat linedef :raise-to-nearest-and-change nil)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-lift (mobj linedef)
  "Linedef type 21"
  (when (emo-player mobj)
    (when (ese-do-plat linedef :down-wait-up-stay nil)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-floor-to-lowest-adjacent-floor (mobj linedef)
  "Linedef type 23"
  (when (emo-player mobj)
    (when (ese-do-floor linedef :lower-floor-to-lowest)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-door (mobj linedef)
  "Linedef type 29"
  (when (emo-player mobj)
    (when (ese-do-door linedef :normal)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-ceiling-to-floor (mobj linedef)
  "Linedef type 41"
  (when (emo-player mobj)
    (when (ese-do-ceiling linedef :lower-to-floor)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-floor-to-8-above-higher-adjacent-floor-fast (mobj linedef)
  "Linedef type 71"
  (when (emo-player mobj)
    (when (ese-do-floor linedef :turbo-lower)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-start-crusher-slow-damage (mobj linedef)
  "Linedef type 49"
  (when (emo-player mobj)
    (when (ese-do-ceiling linedef :crush-and-raise)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-door-close (mobj linedef)
  "Linedef type 50"
  (when (emo-player mobj)
    (when (ese-do-door linedef :close)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-exit-secret (mobj linedef)
  "Linedef type 51"
  (when (emo-player mobj)
    (ese-change-switch-texture linedef nil)
    ;; TODO: secret exit level
    ))
(defun eldoom-special--s1-floor-to-8-below-lowest-adjacent-ceiling-and-crush (mobj linedef)
  "Linedef type 55"
  (when (emo-player mobj)
    (when (ese-do-floor linedef :raise-floor-crush)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-floor-to-lowest-adjacent-ceiling (mobj linedef)
  "Linedef type 101"
  (when (emo-player mobj)
    (when (ese-do-floor linedef :raise-floor)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-floor-to-highest-adjacent-floor (mobj linedef)
  "Linedef type 102"
  (when (emo-player mobj)
    (when (ese-do-floor linedef :lower-floor)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-door-stay-open (mobj linedef)
  "Linedef type 103"
  (when (emo-player mobj)
    (when (ese-do-door linedef :open)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-door-fast (mobj linedef)
  "Linedef type 111"
  (when (emo-player mobj)
    (when (ese-do-door linedef :blaze-raise)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-door-stay-open-fast (mobj linedef)
  "Linedef type 112"
  (when (emo-player mobj)
    (when (ese-do-door linedef :blaze-open)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-door-close-fast (mobj linedef)
  "Linedef type 113"
  (when (emo-player mobj)
    (when (ese-do-door linedef :blaze-close)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-lift-fast (mobj linedef)
  "Linedef type 122"
  (when (emo-player mobj)
    (when (ese-do-plat linedef :blaze-down-wait-up-stay 0)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-build-stairs-16-crush (mobj linedef)
  "Linedef type 127"
  (when (emo-player mobj)
    (when (ese-do-plat linedef :blaze-down-wait-up-stay 0)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-floor-to-higher-floor-fast (mobj linedef)
  "Linedef type 131"
  (when (emo-player mobj)
    (when (ese-do-floor linedef :raise-floor-turbo)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-locked-door-fast (mobj linedef)
  "Linedef types 133 135 137"
  (when (emo-player mobj)
    (when (ese-do-locked-door linedef :blaze-open mobj)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--s1-floor-up-512 (mobj linedef)
  "Linedef type 140"
  (when (emo-player mobj)
    (when (ese-do-floor linedef :raise-floor-512)
      (ese-change-switch-texture linedef nil))
    t))
(defun eldoom-special--sr-door-close (mobj linedef)
  "Linedef type 42"
  (when (emo-player mobj)
    (when (ese-do-door linedef :close)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-ceiling-to-floor (mobj linedef)
  "Linedef type 43"
  (when (emo-player mobj)
    (when (ese-do-ceiling linedef :lower-to-floor)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-floor-to-highest-adjacent-floor (mobj linedef)
  "Linedef type 45"
  (when (emo-player mobj)
    (when (ese-do-floor linedef :lower-floor)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-floor-to-lowest-adjacent-floor (mobj linedef)
  "Linedef type 60"
  (when (emo-player mobj)
    (when (ese-do-floor linedef :lower-floor-to-lowest)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-door-stay-open (mobj linedef)
  "Linedef type 61"
  (when (emo-player mobj)
    (when (ese-do-door linedef :open)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-lift (mobj linedef)
  "Linedef type 62"
  (when (emo-player mobj)
    (when (ese-do-plat linedef :down-wait-up-stay 1)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-door (mobj linedef)
  "Linedef type 63"
  (when (emo-player mobj)
    (when (ese-do-door linedef :normal)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-floor-to-lowest-adjacent-ceiling (mobj linedef)
  "Linedef type 64"
  (when (emo-player mobj)
    (when (ese-do-floor linedef :raise-floor)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-floor-up-24-change-texture (mobj linedef)
  "Linedef type 66"
  (when (emo-player mobj)
    (when (ese-do-plat linedef :raise-and-change 24)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-floor-up-32-change-texture (mobj linedef)
  "Linedef type 67"
  (when (emo-player mobj)
    (when (ese-do-plat linedef :raise-and-change 32)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-floor-to-8-below-lowest-adjacent-ceiling-and-crush (mobj linedef)
  "Linedef type 65"
  (when (emo-player mobj)
    (when (ese-do-floor linedef :raise-floor-crush)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-floor-to-higher-floor-change-texture (mobj linedef)
  "Linedef type 68"
  (when (emo-player mobj)
    (when (ese-do-plat linedef :raise-to-nearest-and-change 0)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-floor-to-higher-adjacent-floor (mobj linedef)
  "Linedef type 69"
  (when (emo-player mobj)
    (when (ese-do-floor linedef :raise-floor-to-nearest)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-floor-to-8-above-higher-adjacent-floor-fast (mobj linedef)
  "Linedef type 70"
  (when (emo-player mobj)
    (when (ese-do-floor linedef :turbo-lower)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-door-fast (mobj linedef)
  "Linedef type 114"
  (when (emo-player mobj)
    (when (ese-do-door linedef :blaze-raise)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-door-stay-open-fast (mobj linedef)
  "Linedef type 115"
  (when (emo-player mobj)
    (when (ese-do-door linedef :blaze-open)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-door-close-fast (mobj linedef)
  "Linedef type 116"
  (when (emo-player mobj)
    (when (ese-do-door linedef :blaze-close)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-lift-fast (mobj linedef)
  "Linedef type 123"
  (when (emo-player mobj)
    (when (ese-do-plat linedef :blaze-down-wait-up-stay 0)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-floor-to-higher-floor-fast (mobj linedef)
  "Linedef type 132"
  (when (emo-player mobj)
    (when (ese-do-floor linedef :raise-floor-turbo)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-locked-door-fast (mobj linedef)
  "Linedef types 99 134 136"
  (when (emo-player mobj)
    (when (ese-do-locked-door linedef :blaze-open mobj)
      (ese-change-switch-texture linedef t))
    t))
(defun eldoom-special--sr-light-to-255 (mobj linedef)
  "Linedef type 138"
  (when (emo-player mobj)
    (ese-light-turn-on linedef 255)
    (ese-change-switch-texture linedef t)
    t))
(defun eldoom-special--sr-light-to-35 (mobj linedef)
  "Linedef type 139"
  (when (emo-player mobj)
    (ese-light-turn-on linedef 35)
    (ese-change-switch-texture linedef t)
    t))
(defun eldoom-special--on-use (special)
  (cl-case special
    ((1 32 33 34) #'eldoom-special--door-monster-can-use)
    ((26 27 28 31 117 118) #'eldoom-special--door)
    (7 #'eldoom-special--s1-build-stairs-8-up)
    (9 #'eldoom-special--s1-floor-donut)
    (11 #'eldoom-special--s1-exit-normal)
    (14 #'eldoom-special--s1-floor-up-32-change-texture)
    (15 #'eldoom-special--s1-floor-up-24-change-texture)
    (18 #'eldoom-special--s1-floor-to-higher-adjacent-floor)
    (20 #'eldoom-special--s1-floor-to-higher-floor-change-texture)
    (21 #'eldoom-special--s1-lift)
    (23 #'eldoom-special--s1-floor-to-lowest-adjacent-floor)
    (29 #'eldoom-special--s1-door)
    (41 #'eldoom-special--s1-ceiling-to-floor)
    (71 #'eldoom-special--s1-floor-to-8-above-higher-adjacent-floor-fast)
    (49 #'eldoom-special--s1-start-crusher-slow-damage)
    (50 #'eldoom-special--s1-door-close)
    (51 #'eldoom-special--s1-exit-secret)
    (55 #'eldoom-special--s1-floor-to-8-below-lowest-adjacent-ceiling-and-crush)
    (101 #'eldoom-special--s1-floor-to-lowest-adjacent-ceiling)
    (102 #'eldoom-special--s1-floor-to-highest-adjacent-floor)
    (103 #'eldoom-special--s1-door-stay-open)
    (111 #'eldoom-special--s1-door-fast)
    (112 #'eldoom-special--s1-door-stay-open-fast)
    (113 #'eldoom-special--s1-door-close-fast)
    (122 #'eldoom-special--s1-lift-fast)
    (127 #'eldoom-special--s1-build-stairs-16-crush)
    (131 #'eldoom-special--s1-floor-to-higher-floor-fast)
    ((133 135 137) #'eldoom-special--s1-locked-door-fast)
    (140 #'eldoom-special--s1-floor-up-512)
    ;; buttons
    (42 #'eldoom-special--sr-door-close)
    (43 #'eldoom-special--sr-ceiling-to-floor)
    (45 #'eldoom-special--sr-floor-to-highest-adjacent-floor)
    (60 #'eldoom-special--sr-floor-to-lowest-adjacent-floor)
    (61 #'eldoom-special--sr-door-stay-open)
    (62 #'eldoom-special--sr-lift)
    (63 #'eldoom-special--sr-door)
    (64 #'eldoom-special--sr-floor-to-lowest-adjacent-ceiling)
    (66 #'eldoom-special--sr-floor-up-24-change-texture)
    (67 #'eldoom-special--sr-floor-up-32-change-texture)
    (65 #'eldoom-special--sr-floor-to-8-below-lowest-adjacent-ceiling-and-crush)
    (68 #'eldoom-special--sr-floor-to-higher-floor-change-texture)
    (69 #'eldoom-special--sr-floor-to-higher-adjacent-floor)
    (70 #'eldoom-special--sr-floor-to-8-above-higher-adjacent-floor-fast)
    (114 #'eldoom-special--sr-door-fast)
    (115 #'eldoom-special--sr-door-stay-open-fast)
    (116 #'eldoom-special--sr-door-close-fast)
    (123 #'eldoom-special--sr-lift-fast)
    (132 #'eldoom-special--sr-floor-to-higher-floor-fast)
    ((99 134 136) #'eldoom-special--sr-locked-door-fast)
    (138 #'eldoom-special--sr-light-to-255)
    (139 #'eldoom-special--sr-light-to-35)))

(defun eldoom-special--projectilep (mobj)
  (let ((type (emo-type mobj)))
    (or (= type enf-MT_ROCKET)
        (= type enf-MT_PLASMA)
        (= type enf-MT_BFG)
        (= type enf-MT_TROOPSHOT)
        (= type enf-MT_HEADSHOT)
        (= type enf-MT_BRUISERSHOT))))

;; Walk over lines
(defun eldoom-special--w1-door-stay-open (mobj _from-side linedef)
  "Linedef type 2"
  (when (emo-player mobj)
    (ese-do-door linedef :open)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-door-close (mobj _from-side linedef)
  "Linedef type 3"
  (when (emo-player mobj)
    (ese-do-door linedef :close)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-door (mobj _from-side linedef)
  "Linedef type 4"
  (when (not (eldoom-special--projectilep mobj))
    (ese-do-door linedef :normal)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-floor-to-lowest-adjacent-ceiling (mobj _from-side linedef)
  "Linedef type 5"
  (when (emo-player mobj)
    (ese-do-floor linedef :raise-floor)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-start-crusher-fast-damage (mobj _from-side linedef)
  "Linedef type 6"
  (when (emo-player mobj)
    (ese-do-ceiling linedef :fast-crush-and-raise)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-build-stairs-8-up (mobj _from-side linedef)
  "Linedef type 8"
  (when (emo-player mobj)
    (ese-build-stairs linedef :fast-crush-and-raise)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-lift-also-monsters (mobj _from-side linedef)
  "Linedef type 10"
  (when (not (eldoom-special--projectilep mobj))
    (ese-do-plat linedef :down-wait-up-stay 0)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-light-to-highest-adjacent-level (mobj _from-side linedef)
  "Linedef type 12"
  (when (emo-player mobj)
    (ese-light-turn-on linedef 0)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-light-to-255 (mobj _from-side linedef)
  "Linedef type 13"
  (when (emo-player mobj)
    (ese-light-turn-on linedef 255)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-door-close-and-open (mobj _from-side linedef)
  "Linedef type 16"
  (when (emo-player mobj)
    (ese-do-door linedef :close-30-then-open)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-light-blink-1-0-sec (mobj _from-side linedef)
  "Linedef type 17"
  (when (emo-player mobj)
    (ese-start-light-strobing linedef)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-floor-to-highest-adjacent-floor (mobj _from-side linedef)
  "Linedef type 19"
  (when (emo-player mobj)
    (ese-do-floor linedef :lower-floor)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-floor-to-higher-floor-change-texture (mobj _from-side linedef)
  "Linedef type 22"
  (when (emo-player mobj)
    (ese-do-plat linedef :raise-to-nearest-and-change 0)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-start-crusher-slow-damage (mobj _from-side linedef)
  "Linedef type 25"
  (when (emo-player mobj)
    (ese-do-ceiling linedef :crush-and-raise)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-floor-up-shortest-lower-texture (mobj _from-side linedef)
  "Linedef type 30"
  (when (emo-player mobj)
    (ese-do-floor linedef :raise-to-texture)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-light-to-35 (mobj _from-side linedef)
  "Linedef type 35"
  (when (emo-player mobj)
    (ese-light-turn-on linedef 35)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-floor-to-8-above-highest-adjacent-floor-fast (mobj _from-side linedef)
  "Linedef type 36"
  (when (emo-player mobj)
    (ese-do-floor linedef :turbo-lower)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-floor-to-lowest-adjacent-floor-change-texture-and-type (mobj _from-side linedef)
  "Linedef type 37"
  (when (emo-player mobj)
    (ese-do-floor linedef :lower-and-change)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-floor-to-lowest-adjacent-floor (mobj _from-side linedef)
  "Linedef type 38"
  (when (emo-player mobj)
    (ese-do-floor linedef :lower-floor-to-lowest)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-teleport (mobj from-side linedef)
  "Linedef type 39"
  (when (not (eldoom-special--projectilep mobj))
    (ese-teleport linedef from-side mobj)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-ceiling-to-highest-ceiling (mobj _from-side linedef)
  "Linedef type 40"
  (when (emo-player mobj)
    (ese-do-ceiling linedef :raise-to-highest)
    (ese-do-floor linedef :lower-floor-to-lowest)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-ceiling-to-8-above-floor (mobj _from-side linedef)
  "Linedef type 44"
  (when (emo-player mobj)
    (ese-do-ceiling linedef :lower-and-crush)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-exit-normal (_mobj _linedef)
  "Linedef type 52"
  ;; TODO: exit level
  )
(defun eldoom-special--w1-start-moving-floor (mobj _from-side linedef)
  "Linedef type 53"
  (when (emo-player mobj)
    (ese-do-plat linedef :perpetual-raise 0)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-stop-moving-floor (mobj _from-side linedef)
  "Linedef type 54"
  (when (emo-player mobj)
    (ese-stop-plat linedef)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-floor-to-8-below-lowest-adjacent-ceiling-and-crush (mobj _from-side linedef)
  "Linedef type 56"
  (when (emo-player mobj)
    (ese-do-floor linedef :raise-floor-crush)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-stop-crusher (mobj _from-side linedef)
  "Linedef type 57"
  (when (emo-player mobj)
    (ese-ceiling-crush-stop linedef)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-floor-up-24 (mobj _from-side linedef)
  "Linedef type 58"
  (when (emo-player mobj)
    (ese-do-floor linedef :raise-floor-24)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-floor-up-24-change-texture-and-type (mobj _from-side linedef)    
  "Linedef type 59"
  (when (emo-player mobj)
    (ese-do-floor linedef :raise-floor-24-and-change)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-build-stairs-16-and-crush (mobj _from-side linedef)
  "Linedef type 100"
  (when (emo-player mobj)
    (ese-build-stairs linedef :turbo-16)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-light-to-lowest-adjacent-level (mobj _from-side linedef)
  "Linedef type 104"
  (when (emo-player mobj)
    (ese-turn-tag-lights-off linedef)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-door-fast (mobj _from-side linedef)
  "Linedef type 108"
  (when (emo-player mobj)
    (ese-do-door linedef :blaze-raise)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-door-stay-open-fast (mobj _from-side linedef)
  "Linedef type 109"
  (when (emo-player mobj)
    (ese-do-door linedef :blaze-open)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-door-close-fast (mobj _from-side linedef)
  "Linedef type 110"
  (when (emo-player mobj)
    (ese-do-door linedef :blaze-close)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-floor-to-higher-adjacent-floor (mobj _from-side linedef)
  "Linedef type 119"
  (when (emo-player mobj)
    (ese-do-floor linedef :raise-floor-to-nearest)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-lift-fast (mobj _from-side linedef)
  "Linedef type 121"
  (when (emo-player mobj)
    (ese-do-plat linedef :blaze-down-wait-up-stay 0)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-exit-secret (_mobj _linedef)
  "Linedef type 124"
  ;; TODO 
  )
(defun eldoom-special--w1-teleport-monsters-only (mobj from-side linedef)
  "Linedef type 125"
  (when (and (not (emo-player mobj)) (not (eldoom-special--projectilep mobj)))
    (ese-teleport linedef from-side mobj)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-floor-to-higher-floor-fast (mobj _from-side linedef)
  "Linedef type 130"
  (when (emo-player mobj)
    (ese-do-floor linedef :raise-floor-turbo)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--w1-start-crusher-silent (mobj _from-side linedef)
  "Linedef type 141"
  (when (emo-player mobj)
    (ese-do-ceiling linedef :silent-crush-and-raise)
    (setf (eldoom-wad-linedef-special linedef) 0
          (eldoom-wad-linedef-on-walk linedef) nil)))
(defun eldoom-special--wr-ceiling-to-8-above-floor (mobj _from-side linedef)
  "Linedef type 72"
  (when (emo-player mobj)
    (ese-do-ceiling linedef :lower-and-crush)))
(defun eldoom-special--wr-start-crusher-slow-damage (mobj _from-side linedef)
  "Linedef type 73"
  (when (emo-player mobj)
    (ese-do-ceiling linedef :crush-and-raise)))
(defun eldoom-special--wr-stop-crusher (mobj _from-side linedef)
  "Linedef type 74"
  (when (emo-player mobj)
    (ese-ceiling-crush-stop linedef)))
(defun eldoom-special--wr-door-close (mobj _from-side linedef)
  "Linedef type 75"
  (when (emo-player mobj)
    (ese-do-door linedef :close)))
(defun eldoom-special--wr-door-close-and-open (mobj _from-side linedef)
  "Linedef type 76"
  (when (emo-player mobj)
    (ese-do-door linedef :close-30-then-open)))
(defun eldoom-special--wr-start-crusher-fast-damage (mobj _from-side linedef)
  "Linedef type 77"
  (when (emo-player mobj)
    (ese-do-ceiling linedef :fast-crush-and-raise)))
(defun eldoom-special--wr-light-to-35 (mobj _from-side linedef)
  "Linedef type 79"
  (when (emo-player mobj)
    (ese-light-turn-on linedef 35)))
(defun eldoom-special--wr-light-to-highest-adjacent-level (mobj _from-side linedef)
  "Linedef type 80"
  (when (emo-player mobj)
    (ese-light-turn-on linedef 0)))
(defun eldoom-special--wr-light-to-255 (mobj _from-side linedef)
  "Linedef type 81"
  (when (emo-player mobj)
    (ese-light-turn-on linedef 255)))
(defun eldoom-special--wr-floor-to-lowest-adjacent-floor (mobj _from-side linedef)
  "Linedef type 82"
  (when (emo-player mobj)
    (ese-do-floor linedef :lower-floor-to-lowest)))
(defun eldoom-special--wr-floor-to-highest-adjacent-floor (mobj _from-side linedef)
  "Linedef type 83"
  (when (emo-player mobj)
    (ese-do-floor linedef :lower-floor)))
(defun eldoom-special--wr-floor-to-lowest-adjacent-floor-change-texture-and-type (mobj _from-side linedef)
  "Linedef type 84"
  (when (emo-player mobj)
    (ese-do-floor linedef :lower-and-change)))
(defun eldoom-special--wr-door-stay-open (mobj _from-side linedef)
  "Linedef type 86"
  (when (emo-player mobj)
    (ese-do-door linedef :open)))
(defun eldoom-special--wr-start-moving-floor (mobj _from-side linedef)
  "Linedef type 87"
  (when (emo-player mobj)
    (ese-do-plat linedef :perpetual-raise 0)))
(defun eldoom-special--wr-lift-also-monsters (mobj _from-side linedef)
  "Linedef type 88"
  (when (not (eldoom-special--projectilep mobj))
    (ese-do-plat linedef :down-wait-up-stay 0)))
(defun eldoom-special--wr-stop-moving-floor (mobj _from-side linedef)
  "Linedef type 89"
  (when (emo-player mobj)
    (ese-stop-plat linedef)))
(defun eldoom-special--wr-door (mobj _from-side linedef)
  "Linedef type 90"
  (when (emo-player mobj)
    (ese-do-door linedef :normal)))
(defun eldoom-special--wr-floor-to-lowest-adjacent-ceiling (mobj _from-side linedef)
  "Linedef type 91"
  (when (emo-player mobj)
    (ese-do-floor linedef :raise-floor)))
(defun eldoom-special--wr-floor-up-24 (mobj _from-side linedef)
  "Linedef type 92"
  (when (emo-player mobj)
    (ese-do-floor linedef :raise-floor-24)))
(defun eldoom-special--wr-floor-up-24-change-texture-and-type (mobj _from-side linedef)
  "Linedef type 93"
  (when (emo-player mobj)
    (ese-do-floor linedef :raise-floor-24-and-change)))
(defun eldoom-special--wr-floor-to-8-below-lowest-adjacent-ceiling-and-crush (mobj _from-side linedef)
  "Linedef type 94"
  (when (emo-player mobj)
    (ese-do-floor linedef :raise-floor-crush)))
(defun eldoom-special--wr-floor-to-higher-floor-change-texture (mobj _from-side linedef)
  "Linedef type 95"
  (when (emo-player mobj)
    (ese-do-plat linedef :raise-to-nearest-and-change 0)))
(defun eldoom-special--wr-floor-up-shortest-lower-texture (mobj _from-side linedef)
  "Linedef type 96"
  (when (emo-player mobj)
    (ese-do-floor linedef :raise-to-texture)))
(defun eldoom-special--wr-teleport (mobj from-side linedef)
  "Linedef type 97"
  (when (not (eldoom-special--projectilep mobj))
    (ese-teleport linedef from-side mobj)))
(defun eldoom-special--wr-floor-to-8-above-highest-adjacent-floor-fast (mobj _from-side linedef)
  "Linedef type 98"
  (when (emo-player mobj)
    (ese-do-floor linedef :turbo-lower)))
(defun eldoom-special--wr-door-fast (mobj _from-side linedef)
  "Linedef type 105"
  (when (emo-player mobj)
    (ese-do-door linedef :blaze-raise)))
(defun eldoom-special--wr-door-stay-open-fast (mobj _from-side linedef)
  "Linedef type 106"
  (when (emo-player mobj)
    (ese-do-door linedef :blaze-open)))
(defun eldoom-special--wr-door-close-fast (mobj _from-side linedef)
  "Linedef type 107"
  (when (emo-player mobj)
    (ese-do-door linedef :blaze-close)))
(defun eldoom-special--wr-lift-fast (mobj _from-side linedef)
  "Linedef type 120"
  (when (emo-player mobj)
    (ese-do-plat linedef :blaze-down-wait-up-stay 0)))
(defun eldoom-special--wr-teleport-monsters-only (mobj from-side linedef)
  "Linedef type 126"
  (when (and (not (emo-player mobj)) (not (eldoom-special--projectilep mobj)))
    (ese-teleport linedef from-side mobj)))
(defun eldoom-special--wr-floor-to-higher-adjacent-floor (mobj _from-side linedef)
  "Linedef type 128"
  (when (emo-player mobj)
    (ese-do-floor linedef :raise-floor-to-nearest)))
(defun eldoom-special--wr-floor-to-higher-floor-fast (mobj _from-side linedef)
  "Linedef type 129"
  (when (emo-player mobj)
    (ese-do-floor linedef :raise-floor-turbo)))
(defun eldoom-special--on-walk (special)
  (cl-case special
    (2 #'eldoom-special--w1-door-stay-open)
    (3 #'eldoom-special--w1-door-close)
    (4 #'eldoom-special--w1-door)
    (5 #'eldoom-special--w1-floor-to-lowest-adjacent-ceiling)
    (6 #'eldoom-special--w1-start-crusher-fast-damage)
    (8 #'eldoom-special--w1-build-stairs-8-up)
    (10 #'eldoom-special--w1-lift-also-monsters)
    (12 #'eldoom-special--w1-light-to-highest-adjacent-level)
    (13 #'eldoom-special--w1-light-to-255)
    (16 #'eldoom-special--w1-door-close-and-open)
    (17 #'eldoom-special--w1-light-blink-1-0-sec)
    (19 #'eldoom-special--w1-floor-to-highest-adjacent-floor)
    (22 #'eldoom-special--w1-floor-to-higher-floor-change-texture)
    (25 #'eldoom-special--w1-start-crusher-slow-damage)
    (30 #'eldoom-special--w1-floor-up-shortest-lower-texture)
    (35 #'eldoom-special--w1-light-to-35)
    (36 #'eldoom-special--w1-floor-to-8-above-highest-adjacent-floor-fast)
    (37 #'eldoom-special--w1-floor-to-lowest-adjacent-floor-change-texture-and-type)
    (38 #'eldoom-special--w1-floor-to-lowest-adjacent-floor)
    (39 #'eldoom-special--w1-teleport)
    (40 #'eldoom-special--w1-ceiling-to-highest-ceiling)
    (44 #'eldoom-special--w1-ceiling-to-8-above-floor)
    (52 #'eldoom-special--w1-exit-normal)
    (53 #'eldoom-special--w1-start-moving-floor)
    (54 #'eldoom-special--w1-stop-moving-floor)
    (56 #'eldoom-special--w1-floor-to-8-below-lowest-adjacent-ceiling-and-crush)
    (57 #'eldoom-special--w1-stop-crusher)
    (58 #'eldoom-special--w1-floor-up-24)
    (59 #'eldoom-special--w1-floor-up-24-change-texture-and-type)
    (100 #'eldoom-special--w1-build-stairs-16-and-crush)
    (104 #'eldoom-special--w1-light-to-lowest-adjacent-level)
    (108 #'eldoom-special--w1-door-fast)
    (109 #'eldoom-special--w1-door-stay-open-fast)
    (110 #'eldoom-special--w1-door-close-fast)
    (119 #'eldoom-special--w1-floor-to-higher-adjacent-floor)
    (121 #'eldoom-special--w1-lift-fast)
    (124 #'eldoom-special--w1-exit-secret)
    (125 #'eldoom-special--w1-teleport-monsters-only)
    (130 #'eldoom-special--w1-floor-to-higher-floor-fast)
    (141 #'eldoom-special--w1-start-crusher-silent)
    ;; retriggers
    (72 #'eldoom-special--wr-ceiling-to-8-above-floor)
    (73 #'eldoom-special--wr-start-crusher-slow-damage)
    (74 #'eldoom-special--wr-stop-crusher)
    (75 #'eldoom-special--wr-door-close)
    (76 #'eldoom-special--wr-door-close-and-open)
    (77 #'eldoom-special--wr-start-crusher-fast-damage)
    (79 #'eldoom-special--wr-light-to-35)
    (80 #'eldoom-special--wr-light-to-highest-adjacent-level)
    (81 #'eldoom-special--wr-light-to-255)
    (82 #'eldoom-special--wr-floor-to-lowest-adjacent-floor)
    (83 #'eldoom-special--wr-floor-to-highest-adjacent-floor)
    (84 #'eldoom-special--wr-floor-to-lowest-adjacent-floor-change-texture-and-type)
    (86 #'eldoom-special--wr-door-stay-open)
    (87 #'eldoom-special--wr-start-moving-floor)
    (88 #'eldoom-special--wr-lift-also-monsters)
    (89 #'eldoom-special--wr-stop-moving-floor)
    (90 #'eldoom-special--wr-door)
    (91 #'eldoom-special--wr-floor-to-lowest-adjacent-ceiling)
    (92 #'eldoom-special--wr-floor-up-24)
    (93 #'eldoom-special--wr-floor-up-24-change-texture-and-type)
    (94 #'eldoom-special--wr-floor-to-8-below-lowest-adjacent-ceiling-and-crush)
    (95 #'eldoom-special--wr-floor-to-higher-floor-change-texture)
    (96 #'eldoom-special--wr-floor-up-shortest-lower-texture)
    (97 #'eldoom-special--wr-teleport)
    (98 #'eldoom-special--wr-floor-to-8-above-highest-adjacent-floor-fast)
    (105 #'eldoom-special--wr-door-fast)
    (106 #'eldoom-special--wr-door-stay-open-fast)
    (107 #'eldoom-special--wr-door-close-fast)
    (120 #'eldoom-special--wr-lift-fast)
    (126 #'eldoom-special--wr-teleport-monsters-only)
    (128 #'eldoom-special--wr-floor-to-higher-adjacent-floor)
    (129 #'eldoom-special--wr-floor-to-higher-floor-fast)))

(defun eldoom-special--g1-floor-to-lowest-adjacent-ceiling (mobj linedef)
  "Linedef type 24"
  (when (emo-player mobj)
    (ese-do-floor linedef :raise-floor)
    (ese-change-switch-texture linedef nil)))
(defun eldoom-special--gr-door-also-monsters (_mobj linedef)
  "Linedef type 46"
  (ese-do-door linedef :open)
  (ese-change-switch-texture linedef t))
(defun eldoom-special--g1-floor-to-higher-floor-change-texture (mobj linedef)
  "Linedef 47"
  (when (emo-player mobj)
    (ese-do-plat linedef :raise-to-nearest-and-change 0)
    (ese-change-switch-texture linedef nil)))

(defun eldoom-special--on-shoot (special)
  (cl-case special
    (24 #'eldoom-special--g1-floor-to-lowest-adjacent-ceiling)
    (46 #'eldoom-special--gr-door-also-monsters)
    (47 #'eldoom-special--g1-floor-to-higher-floor-change-texture)))

(defun eldoom-special--pick-up-green-armor (player _pickup)
  (when (emo--give-armor player 1)
    (setf (emo-player-message player) "Picked up the armor.")
    t))

(defun eldoom-special--pick-up-blue-armor (player _pickup)
  (when (emo--give-armor player 2)
    (setf (emo-player-message player) "Picked up the MegaArmor!")
    t))

(defun eldoom-special--pick-up-health-bonus (player _pickup)
  (setf (emo-player-health player) (min (1+ (emo-player-health player)) 200)
        (emo-health (emo-player-mobj player)) (emo-player-health player)
        (emo-player-message player) "Picked up a health bonus.")
  t)

(defun eldoom-special--pick-up-armor-bonus (player _pickup)
  (setf (emo-player-armor-points player) (min (1+ (emo-player-armor-points player)) 200)
        (emo-player-message player) "Picked up an armor bonus.")
  t)

(defun eldoom-special--pick-up-soul-sphere (player _pickup)
  (setf (emo-player-health player) (min (+ 100 (emo-player-health player)) 200)
        (emo-health (emo-player-mobj player)) (emo-player-health player)
        (emo-player-message player) "Supercharge!")
  t)

(defun eldoom-special--pick-up-mega-sphere (player _pickup)
  (emo--give-armor player 2)
  (setf (emo-player-health player) 200
        (emo-health (emo-player-mobj player)) (emo-player-health player)
        (emo-player-message player) "MegaSphere!")
  t)

(defun eldoom-special--pick-up-blue-keycard (player _pickup)
  (when (not (memq :blue-card (emo-player-cards player)))
    (setf (emo-player-message player) "Picked up a blue keycard."))
  (emo--give-card player :blue-card)
  t)

(defun eldoom-special--pick-up-yellow-keycard (player _pickup)
  (when (not (memq :yellow-card (emo-player-cards player)))
    (setf (emo-player-message player) "Picked up a yellow keycard."))
  (emo--give-card player :yellow-card)
  t)

(defun eldoom-special--pick-up-red-keycard (player _pickup)
  (when (not (memq :red-card (emo-player-cards player)))
    (setf (emo-player-message player) "Picked up a red keycard."))
  (emo--give-card player :red-card)
  t)

(defun eldoom-special--pick-up-blue-skull-key (player _pickup)
  (when (not (memq :blue-skull (emo-player-cards player)))
    (setf (emo-player-message player) "Picked up a blue skull key."))
  (emo--give-card player :blue-skull)
  t)

(defun eldoom-special--pick-up-yellow-skull-key (player _pickup)
  (when (not (memq :yellow-skull (emo-player-cards player)))
    (setf (emo-player-message player) "Picked up a yellow skull key."))
  (emo--give-card player :yellow-skull)
  t)

(defun eldoom-special--pick-up-red-skull-key (player _pickup)
  (when (not (memq :red-skull (emo-player-cards player)))
    (setf (emo-player-message player) "Picked up a red skull key."))
  (emo--give-card player :red-skull)
  t)

(defun eldoom-special--pick-up-stimpack (player _pickup)
  (when (emo--give-body player 10)
    (setf (emo-player-message player) "Picked up a stimpack.")
    t))

(defun eldoom-special--pick-up-medikit (player _pickup)
  (when (emo--give-body player 10)
    (setf (emo-player-message player)
          (if (< (emo-player-health player) 25)
              "Picked up a medikit that you REALLY need!"
            "Picked up a medikit."))
    t))

(defun eldoom-special--pick-up-invulnerability (player _pickup)
  (when (emo--give-power player eldoom-globals-pw_invulnerability)
    (setf (emo-player-message player) "Invulnerability!")
    t))

(defun eldoom-special--pick-up-berserk (player _pickup)
  (when (emo--give-power player eldoom-globals-pw_strength)
    (setf (emo-player-message player) "Berserk!")
    (when (/= (emo-player-ready-weapon player) eldoom-globals-wp_fist)
      (setf (emo-player-pending-weapon player) eldoom-globals-wp_fist))
    t))

(defun eldoom-special--pick-up-invisibility (player _pickup)
  (when (emo--give-power player eldoom-globals-pw_invisibility)
    (setf (emo-player-message player) "Partial Invisibility!")
    t))

(defun eldoom-special--pick-up-radiation-suit (player _pickup)
  (when (emo--give-power player eldoom-globals-pw_ironfeet)
    (setf (emo-player-message player) "Radiation Shielding Suit")
    t))

(defun eldoom-special--pick-up-computer-area-map (player _pickup)
  (when (emo--give-power player eldoom-globals-pw_allmap)
    (setf (emo-player-message player) "Computer Area Map")
    t))

(defun eldoom-special--pick-up-light-amplification (player _pickup)
  (when (emo--give-power player eldoom-globals-pw_infrared)
    (setf (emo-player-message player) "Light Amplification Visor")
    t))

(defun eldoom-special--pick-up-clip (player pickup)
  (let ((amount (if (eldoom-math-has-flag (emo-flags pickup) enf-MF_DROPPED) 0 1)))
    (when (emo--give-ammo player eldoom-globals-am_clip amount)
      (setf (emo-player-message player) "Picked up a clip.")
      t)))

(defun eldoom-special--pick-up-box-of-bullets (player _pickup)
  (when (emo--give-ammo player eldoom-globals-am_clip 5)
    (setf (emo-player-message player) "Picked up a box of bullets.")
    t))

(defun eldoom-special--pick-up-rocket (player _pickup)
  (when (emo--give-ammo player eldoom-globals-am_missile 1)
    (setf (emo-player-message player) "Picked up a rocket.")
    t))

(defun eldoom-special--pick-up-box-of-rockets (player _pickup)
  (when (emo--give-ammo player eldoom-globals-am_missile 5)
    (setf (emo-player-message player) "Picked up a box of rockets.")
    t))

(defun eldoom-special--pick-up-energy-cell (player _pickup)
  (when (emo--give-ammo player eldoom-globals-am_cell 1)
    (setf (emo-player-message player) "Picked up an energy cell.")
    t))

(defun eldoom-special--pick-up-energy-cell-pack (player _pickup)
  (when (emo--give-ammo player eldoom-globals-am_cell 5)
    (setf (emo-player-message player) "Picked up an energy cell pack.")
    t))

(defun eldoom-special--pick-up-shotgun-shells (player _pickup)
  (when (emo--give-ammo player eldoom-globals-am_shell 1)
    (setf (emo-player-message player) "Picked up 4 shotgun shells.")
    t))

(defun eldoom-special--pick-up-box-of-shotgun-shells (player _pickup)
  (when (emo--give-ammo player eldoom-globals-am_shell 5)
    (setf (emo-player-message player) "Picked up a box of shotgun shells.")
    t))

(defun eldoom-special--pick-up-backpack (player _pickup)
  (when (not (emo-player-backpack player))
    (setf (emo-player-backpack player) t)
    (let ((max-ammo (emo-player-max-ammo player)))
      (dotimes (i eldoom-globals-num-ammo)
        (aset max-ammo i (* 2 (aref max-ammo i))))))
  (dotimes (i eldoom-globals-num-ammo)
    (emo--give-ammo player i 1))
  (setf (emo-player-message player) "Picked up a backpack full of ammo!")
  t)

(defun eldoom-special--pick-up-bfg (player _pickup)
  (when (emo--give-weapon player eldoom-globals-wp_bfg nil)
    (setf (emo-player-message player) "You got the BFG9000!  Oh, yes.")
    t))

(defun eldoom-special--pick-up-chaingun (player pickup)
  (let ((amount (if (eldoom-math-has-flag (emo-flags pickup) enf-MF_DROPPED) 0 1)))
    (when (emo--give-weapon player eldoom-globals-wp_chaingun amount)
      (setf (emo-player-message player) "You got the chaingun!")
      t)))

(defun eldoom-special--pick-up-chainsaw (player _pickup)
  (when (emo--give-weapon player eldoom-globals-wp_chainsaw nil)
    (setf (emo-player-message player) "A chainsaw!  Find some meat!")
    t))

(defun eldoom-special--pick-up-rocket-launcher (player _pickup)
  (when (emo--give-weapon player eldoom-globals-wp_missile nil)
    (setf (emo-player-message player) "You got the rocket launcher!")
    t))

(defun eldoom-special--pick-up-plasma-gun (player _pickup)
  (when (emo--give-weapon player eldoom-globals-wp_plasma nil)
    (setf (emo-player-message player) "You got the plasma gun!")
    t))

(defun eldoom-special--pick-up-shotgun (player pickup)
  (let ((amount (if (eldoom-math-has-flag (emo-flags pickup) enf-MF_DROPPED) 0 1)))
    (when (emo--give-weapon player eldoom-globals-wp_shotgun amount)
      (setf (emo-player-message player) "You got the shotgun!")
      t)))

(defun eldoom-special--pick-up-super-shotgun (player pickup)
  (let ((amount (if (eldoom-math-has-flag (emo-flags pickup) enf-MF_DROPPED) 0 1)))
    (when (emo--give-weapon player eldoom-globals-wp_supershotgun amount)
      (setf (emo-player-message player) "You got the super shotgun!")
      t)))

(defun eldoom-special--set-on-pickup (mobj)
  (when (eldoom-math-has-flag (emo-flags mobj) enf-MF_SPECIAL)
    (let ((sprite-index (enf-state-sprite-index (enf-mobj-info-spawn-state (emo-info mobj)))))
      (setf (emo-on-pick-up mobj)
            (cond
             ((= sprite-index enf-SPR_ARM1) #'eldoom-special--pick-up-green-armor)
             ((= sprite-index enf-SPR_ARM2) #'eldoom-special--pick-up-blue-armor)
             ((= sprite-index enf-SPR_BON1) #'eldoom-special--pick-up-health-bonus)
             ((= sprite-index enf-SPR_BON2) #'eldoom-special--pick-up-armor-bonus)
             ((= sprite-index enf-SPR_SOUL) #'eldoom-special--pick-up-soul-sphere)
             ((= sprite-index enf-SPR_MEGA) #'eldoom-special--pick-up-mega-sphere)
             ((= sprite-index enf-SPR_BKEY) #'eldoom-special--pick-up-blue-keycard)
             ((= sprite-index enf-SPR_YKEY) #'eldoom-special--pick-up-yellow-keycard)
             ((= sprite-index enf-SPR_RKEY) #'eldoom-special--pick-up-red-keycard)
             ((= sprite-index enf-SPR_BSKU) #'eldoom-special--pick-up-blue-skull-key)
             ((= sprite-index enf-SPR_YSKU) #'eldoom-special--pick-up-yellow-skull-key)
             ((= sprite-index enf-SPR_RSKU) #'eldoom-special--pick-up-red-skull-key)
             ((= sprite-index enf-SPR_STIM) #'eldoom-special--pick-up-stimpack)
             ((= sprite-index enf-SPR_MEDI) #'eldoom-special--pick-up-medikit)
             ((= sprite-index enf-SPR_PINV) #'eldoom-special--pick-up-invulnerability)
             ((= sprite-index enf-SPR_PSTR) #'eldoom-special--pick-up-berserk)
             ((= sprite-index enf-SPR_PINS) #'eldoom-special--pick-up-invisibility)
             ((= sprite-index enf-SPR_SUIT) #'eldoom-special--pick-up-radiation-suit)
             ((= sprite-index enf-SPR_PMAP) #'eldoom-special--pick-up-computer-area-map)
             ((= sprite-index enf-SPR_PVIS) #'eldoom-special--pick-up-light-amplification)
             ((= sprite-index enf-SPR_CLIP) #'eldoom-special--pick-up-clip)
             ((= sprite-index enf-SPR_AMMO) #'eldoom-special--pick-up-box-of-bullets)
             ((= sprite-index enf-SPR_ROCK) #'eldoom-special--pick-up-rocket)
             ((= sprite-index enf-SPR_BROK) #'eldoom-special--pick-up-box-of-rockets)
             ((= sprite-index enf-SPR_CELL) #'eldoom-special--pick-up-energy-cell)
             ((= sprite-index enf-SPR_CELP) #'eldoom-special--pick-up-energy-cell-pack)
             ((= sprite-index enf-SPR_SHEL) #'eldoom-special--pick-up-shotgun-shells)
             ((= sprite-index enf-SPR_SBOX) #'eldoom-special--pick-up-box-of-shotgun-shells)
             ((= sprite-index enf-SPR_BPAK) #'eldoom-special--pick-up-backpack)
             ((= sprite-index enf-SPR_BFUG) #'eldoom-special--pick-up-bfg)
             ((= sprite-index enf-SPR_MGUN) #'eldoom-special--pick-up-chaingun)
             ((= sprite-index enf-SPR_CSAW) #'eldoom-special--pick-up-chainsaw)
             ((= sprite-index enf-SPR_LAUN) #'eldoom-special--pick-up-rocket-launcher)
             ((= sprite-index enf-SPR_PLAS) #'eldoom-special--pick-up-plasma-gun)
             ((= sprite-index enf-SPR_SHOT) #'eldoom-special--pick-up-shotgun)
             ((= sprite-index enf-SPR_SGN2) #'eldoom-special--pick-up-super-shotgun)
             (t (error "Unknown pickup %d" sprite-index)))))))

(defvar-local eldoom-special--lines nil)

(defun eldoom-special-init (map)
  (let ((special-lines ()))
    (cl-loop for linedef across (eldoom-wad-map-linedefs map) do
             (let ((special (eldoom-wad-linedef-special linedef)))
               (setf (eldoom-wad-linedef-on-use linedef) (eldoom-special--on-use special)
                     (eldoom-wad-linedef-on-walk linedef) (eldoom-special--on-walk special)
                     (eldoom-wad-linedef-on-shoot linedef) (eldoom-special--on-shoot special))
               (when (= special 48) ;; scrolling texture
                 (setq special-lines (cons linedef special-lines)))))
    (setq eldoom-special--lines (vconcat special-lines)))
  (cl-loop for sector across (eldoom-wad-map-sectors map) do
           (cl-case (eldoom-wad-sector-special sector)
             (0 nil)
             (1 (ese-spawn-light-flash sector))
             (2 (ese-spawn-strobe-flash sector ese-strobe-fast-dark-time nil))
             (3 (ese-spawn-strobe-flash sector ese-strobe-slow-dark-time nil))
             (4 (ese-spawn-strobe-flash sector ese-strobe-slow-dark-time nil)
                (setf (eldoom-wad-sector-special sector) 4))
             (8 (ese-spawn-glowing-light sector))
             (9 nil) ;; TODO: count secret sectors
             (10 (ese-spawn-door-close-in-30 sector))
             (12 (ese-spawn-strobe-flash sector ese-strobe-slow-dark-time t))
             (13 (ese-spawn-strobe-flash sector ese-strobe-fast-dark-time t))
             (14 (ese-spawn-door-raise-in-5-minutes sector))
             (17 (ese-spawn-fire-flicker sector))))
  
  (emo-add-spawn-function #'eldoom-special--set-on-pickup))

(defun eldoom-special-update ()
  (cl-loop for linedef across eldoom-special--lines do
           (when (= 48 (eldoom-wad-linedef-special linedef))
             (let ((sidedef (eldoom-wad-linedef-front-sidedef linedef)))
               (cl-incf (eldoom-wad-sidedef-texture-offset sidedef) eldoom-math-unit))))
  (ese-update-specials))

;; Local Variables:
;; read-symbol-shorthands: (("emo-" . "eldoom-mobj-") ("ese-" . "eldoom-sector-") ("enf-" . "eldoom-info-"))
;; End:
(provide 'eldoom-special)
;;; eldoom-special.el ends here
