;;; eldoom-sector.el --- Sector thinkers (doors, platforms) -*- lexical-binding:t -*-

;; Copyright (C) 2026 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;; Door and platform simulation on the global world thinker list
;; (vanilla p_doors.c / p_plats.c).

;;; Code:

(require 'cl-lib)
(require 'eldoom-math)
(require 'eldoom-globals)
(require 'eldoom-info)
(require 'eldoom-intrusive)
(require 'eldoom-map)
(require 'eldoom-mobj)
(require 'eldoom-phys)
(require 'eldoom-random)
(require 'eldoom-think)
(require 'eldoom-wad)

(defconst ese--vertical-door-speed (eldoom-math-fixed-from-int 2))
(defconst ese--floor-speed eldoom-math-unit)
(defconst ese--ceiling-speed eldoom-math-unit)
(defconst ese--plat-speed eldoom-math-unit)

(defconst ese-vertical-door-wait 150)
(defconst ese-plat-wait 3)

(defconst ese--use-range-fx (eldoom-math-fixed-from-int 64)
  "Maximum reach for using a special linedef (vanilla USERANGE).")

;; Linedef types
(defconst ese-lt-door 1)
(defconst ese-lt-build-stairs-switch 7)
(defconst ese-lt-build-stairs-walk 8)
(defconst ese-lt-donut-switch 9)
(defconst ese-lt-blue-door 26)
(defconst ese-lt-yellow-door 27)
(defconst ese-lt-red-door 28)
(defconst ese-lt-door-stay-open 31)
(defconst ese-lt-blue-door-stay-open 32)
(defconst ese-lt-red-door-stay-open 33)
(defconst ese-lt-yellow-door-stay-open 34)
(defconst ese-lt-door-fast 117)
(defconst ese-lt-door-fast-stay-open 118)

(cl-defstruct (ese--door (:constructor ese--make-door) (:copier nil))
  type
  sector
  (top-height 0 :type fixnum)
  (speed 0 :type fixnum)
  direction
  (top-wait 0 :type fixnum)
  (top-count-down 0 :type fixnum)
  thinker)

(cl-defstruct (ese--floor (:constructor ese--make-floor) (:copier nil))
  type
  sector
  (dest-height 0 :type fixnum)
  (speed 0 :type fixnum)
  direction
  (crush nil :type boolean)
  texture
  (new-special 0 :type fixnum)
  thinker)

(cl-defstruct (ese--ceiling (:constructor ese--make-ceiling) (:copier nil))
  (type 0 :type fixnum)
  sector
  bottom-height top-height
  (speed 0 :type fixnum)
  direction old-direction
  (crush nil :type boolean)
  (tag 0 :type fixnum)
  thinker
  next prev)
(define-eldoom-intrusive eldoom-sector--ceiling
                         eldoom-sector--ceiling-prev
                         eldoom-sector--ceiling-next)
(define-eldoom-intrusive-freelist eldoom-sector--ceiling
                                  eldoom-sector--ceiling-next
                                  eldoom-sector--make-ceiling)

(defvar-local ese--active-ceilings nil)

(defun ese--remove-ceiling (ceiling)
  (ese--ceiling-remove ceiling)
  (setf (ese--ceiling-sector ceiling) nil
        (ese--ceiling-thinker ceiling) nil
        (ese--ceiling-prev ceiling) nil)
  (ese--ceiling-move-to-freelist ceiling))

(cl-defstruct (ese--plat (:constructor ese--make-plat) (:copier nil))
  type
  sector
  (bottom-height 0 :type fixnum) (top-height 0 :type fixnum)
  (speed 0 :type fixnum)
  direction old-direction
  (wait 0 :type fixnum)
  (count 0 :type fixnum)
  (tag 0 :type fixnum)
  thinker
  next prev)
(define-eldoom-intrusive eldoom-sector--plat
                         eldoom-sector--plat-prev
                         eldoom-sector--plat-next)
(define-eldoom-intrusive-freelist eldoom-sector--plat
                                  eldoom-sector--plat-next
                                  eldoom-sector--make-plat)

(defvar-local ese--active-plats nil)

(defun ese--remove-plat (plat)
  (ese--plat-remove plat)
  (setf (ese--plat-sector plat) nil
        (ese--plat-thinker plat) nil
        (ese--plat-prev plat) nil
        (ese--plat-wait plat) 0
        (ese--plat-count plat) 0)
  (ese--plat-move-to-freelist plat))

(defun ese--door-special-type (special)
  "Return type as symbol for given door SPECIAL number."
  (cl-case special
    ((1 ;;  ese-lt-door
      26 ;; ese-lt-blue-door
      27 ;; ese-lt-yellow-door
      28) ;; ese-lt-red-door
     :normal)
    ((31 ;; ese-lt-door-stay-open
      32 ;; ese-lt-blue-door-stay-open
      33 ;; ese-lt-red-door-stay-open
      34) ;; ese-lt-yellow-door-stay-open)
     :open)
    (117 ;; ese-lt-door-fast
     :blaze-raise)
    (118 ;; ese-lt-door-fast-stay-open
     :blaze-open)))

(defun ese--door-type-speed (type)
  (cl-ecase type
    ((:normal :open :close :close-30-then-open :raise-in-5-minutes) ese--vertical-door-speed)
    ((:blaze-raise :blaze-open :blaze-close) (* 4 ese--vertical-door-speed))))

(defun ese--door-special-speed (special)
  (ese--door-type-speed (ese--door-special-type special)))

(defun ese--door-type-direction (type)
  (cl-ecase type
    ((:normal :open :blaze-open :blaze-raise) :up)
    ((:close :blaze-close :close-30-then-open) :down)
    (:raise-in-5-minutes :initial-wait)))

(defun ese--door-type-top-height (type sector)
  (cl-ecase type
    ((:normal :open :blaze-open :blaze-raise :close :blaze-close :raise-in-5-minutes)
     (- (emp-lowest-ceiling-surrounding-sector sector) (eldoom-math-fixed-to-int 4)))
    (:close-30-then-open (ewd-sector-ceiling-height-fx sector))))

(defun ese--door-special-top-height (special sector)
  (ese--door-type-top-height (ese--door-special-type special) sector))

(defun ese--floor-type-speed (type)
  (cl-ecase type
    (( :lower-floor :lower-floor-to-lowest :raise-floor-crush :raise-floor :raise-floor-to-nearest
       :raise-floor-24 :raise-floor-512 :raise-floor-24-and-change :raise-to-texture :lower-and-change)
     ese--floor-speed)
    ((:turbo-lower :raise-floor-turbo) (* 4 ese--floor-speed))))

(defun ese--floor-type-direction (type)
  (cl-ecase type
    (( :lower-floor :lower-floor-to-lowest :turbo-lower :lower-and-change) :down)
    (( :raise-floor-crush :raise-floor :raise-floor-to-nearest :raise-floor-24 :raise-floor-512
       :raise-floor-24-and-change :raise-to-texture :raise-floor-turbo) :up)))

(defun ese--floor-type-dest-height (type sector)
  (cl-ecase type
    (:lower-floor
     (emp-highest-floor-surrounding-sector sector))
    (:turbo-lower
     (let ((height (emp-highest-floor-surrounding-sector sector)))
       (when (/= height (ewd-sector-floor-height-fx sector))
         (cl-incf height (eldoom-math-fixed-from-int 8)))
       height))
    ((:lower-floor-to-lowest :lower-and-change) 
     (emp-lowest-floor-surrounding-sector sector))
    ((:raise-floor :raise-floor-crush)
     (let ((height (min (emp-lowest-ceiling-surrounding-sector sector)
                        (ewd-sector-ceiling-height-fx sector))))
       (when (eq type :raise-floor-crush)
         (cl-decf height (eldoom-math-fixed-from-int 8)))
       height))
    ((:raise-floor-turbo :raise-floor-to-nearest)
     (emp-next-highest-floor sector (ewd-sector-floor-height-fx sector)))
    ((:raise-floor-24 :raise-floor-24-and-change)
     (+ (ewd-sector-floor-height-fx sector) (eldoom-math-fixed-from-int 24)))
    (:raise-floor-512
     (+ (ewd-sector-floor-height-fx sector) (eldoom-math-fixed-from-int 512)))
    (:raise-to-texture
     (let ((min-size eldoom-math-int-max))
       (cl-loop for linedef across (ewd-sector-linedefs sector) do
                (when (eldoom-math-has-flag (ewd-linedef-flags linedef) enf-ML_TWOSIDED)
                  (let ((front-side (ewd-linedef-front-sidedef linedef))
                        (back-side (ewd-linedef-back-sidedef linedef)))
                    (when-let* ((texture (ewd-sidedef-lower front-side)))
                      (setq min-size (min min-size (ewd-texture-height texture))))
                    (when-let* ((texture (ewd-sidedef-lower back-side)))
                      (setq min-size (min min-size (ewd-texture-height texture)))))))
       (+ (ewd-sector-floor-height-fx sector) min-size)))))

(defun ese--ceiling-type-speed (type)
  (cl-ecase type
    (:fast-crush-and-raise (* 2 ese--ceiling-speed))
    (( :silent-crush-and-raise :crush-and-raise :lower-and-crush :lower-to-floor :raise-to-highest)
     ese--ceiling-speed)))

(defun ese--ceiling-type-direction (type)
  (cl-ecase type 
    (:raise-to-highest :up)
    (( :fast-crush-and-raise :silent-crush-and-raise :crush-and-raise :lower-and-crush :lower-to-floor)
     :down)))

(defun ese--ceiling-type-top-height (type sector)
  (cl-ecase type
    ((:fast-crush-and-raise :silent-crush-and-raise :crush-and-raise)
     (ewd-sector-ceiling-height-fx sector))
    (:raise-to-highest (emp-highest-ceiling-surrounding-sector sector))
    ((:lower-and-crush :lower-to-floor) 0)))

(defun ese--ceiling-type-bottom-height (type sector)
  (cl-ecase type
    ((:fast-crush-and-raise :lower-to-floor)
     (+ (ewd-sector-floor-height-fx sector) (eldoom-math-fixed-from-int 8)))
    ((:silent-crush-and-raise :crush-and-raise :lower-and-crush)
     (ewd-sector-floor-height-fx sector))
    (:raise-to-highest 0)))

(defun ese--plat-type-top-height (type amount sector)
  (cl-ecase type
    (:raise-to-nearest-and-change
     (emp-next-highest-floor sector (ewd-sector-floor-height-fx sector)))
    (:raise-and-change
     (+ (ewd-sector-floor-height-fx sector) (eldoom-math-fixed-from-int amount)))
    ((:down-wait-up-stay :blaze-down-wait-up-stay)
     (ewd-sector-floor-height-fx sector))
    (:perpetual-raise 
     (max (emp-highest-floor-surrounding-sector sector)
          (ewd-sector-floor-height-fx sector)))))

(defun ese--plat-type-bottom-height (type sector)
  (cl-ecase type
    ((:raise-to-nearest-and-change :raise-and-change) 0)
    ((:down-wait-up-stay :blaze-down-wait-up-stay :perpetual-raise)
     (min (emp-lowest-floor-surrounding-sector sector)
          (ewd-sector-floor-height-fx sector)))))

(defun ese--plat-type-speed (type)
  (cl-ecase type
    ((:raise-to-nearest-and-change :raise-and-change) (/ ese--plat-speed 2))
    ((:down-wait-up-stay :blaze-down-wait-up-stay) (* 4 ese--plat-speed))
    (:perpetual-raise ese--plat-speed)))

(defun ese--plat-type-direction (type)
  (cl-ecase type
    ((:raise-to-nearest-and-change :raise-and-change) :up)
    ((:down-wait-up-stay :blaze-down-wait-up-stay) :down)
    (:perpetual-raise
     (if (zerop (logand (eldoom-random t) 1)) :wait :up))))

(defun ese--plat-type-wait (type)
  (cl-ecase type
    ((:raise-to-nearest-and-change :raise-and-change) 0)
    ((:down-wait-up-stay :blaze-down-wait-up-stay :perpetual-raise) (* 35 ese-plat-wait))))

(defun ese--can-monster-open-closing-p (special)
  (or (= special ese-lt-door)
      (= special ese-lt-blue-door)
      (= special ese-lt-yellow-door)
      (= special ese-lt-red-door)
      (= special ese-lt-door-fast)))

(defun ese--thing-height-clip (mobj)
  "P_ThingHeightClip in p_map.c"
  (let ((on-floor (= (emo-z-fx mobj) (emo-floor-z mobj))))
    (eph-check-position mobj (emo-x-fx mobj) (emo-y-fx mobj))
    (setf (emo-floor-z mobj) eph--tmfloorz-fx
          (emo-ceiling-z mobj) eph--tmceilingz-fx)
    (if on-floor
        (setf (emo-z-fx mobj) (emo-floor-z mobj))
      (when (> (+ (emo-z-fx mobj) (emo-height-fx mobj)) (emo-ceiling-z mobj))
        (setf (emo-z-fx mobj) (- (emo-ceiling-z mobj) (emo-height-fx mobj)))))
    (>= (- (emo-ceiling-z mobj) (emo-floor-z mobj)) (emo-height-fx mobj))))

(defvar-local ese--no-fit nil)

(defun ese--change-sector-callback (mobj crush)
  "PIT_ChangeSector in p_map.c"
  (cond
   ((ese--thing-height-clip mobj) t)
   ((<= (emo-health mobj) 0)
    (emo-set-state mobj (aref enf-states enf-S_GIBS))
    (setf (emo-flags mobj) (logand (emo-flags mobj) (lognot enf-MF_SOLID))
          (emo-height-fx mobj) 0
          (emo-radius-fx mobj) 0))
   ((eldoom-math-has-flag (emo-flags mobj) enf-MF_DROPPED) (emo-remove mobj))
   ((not (eldoom-math-has-flag (emo-flags mobj) enf-MF_SHOOTABLE)) t)
   (t 
    (setq ese--no-fit t)
    (when (and crush (zerop (mod eldoom-globals-level-time 3)))
      (emo-damage mobj nil nil 10)
      (let ((blood (emo-spawn-one (emo-x-fx mobj) (emo-y-fx mobj)
                                  (+ (emo-z-fx mobj) (/ (emo-height-fx mobj) 2))
                                  enf-MT_BLOOD)))
        (setf (emo-momx blood) (ash (- (eldoom-random t) (eldoom-random t)) 12)
              (emo-momy blood) (ash (- (eldoom-random t) (eldoom-random t)) 12)))))))

(defun ese--change-sector (sector crush)
  "P_ChangeSector in p_map.c"
  (setq ese--no-fit nil)
  (let* ((left (ewd-sector-blockbox-left sector))
         (right (ewd-sector-blockbox-right sector))
         (bottom (ewd-sector-blockbox-bottom sector))
         (top (ewd-sector-blockbox-top sector)))
    (cl-loop for x from left to right do
             (cl-loop for y from bottom to top do
                      (emo-for-each-in-block x y #'ese--change-sector-callback crush))))
  ese--no-fit)

(defun ese--move-plane (sector speed dest crush floor-or-ceiling direction)
  "T_MovePlane in p_floor.c"
  (cl-ecase floor-or-ceiling
    (:floor ;; 0
     (let ((last-pos (ewd-sector-floor-height-fx sector))
           (res :ok))
       (cl-case direction
         (:down
          (if (< (- (ewd-sector-floor-height-fx sector) speed) dest)
              (progn
                (setf (ewd-sector-floor-height-fx sector) dest
                      res :past-destination)
                (when (ese--change-sector sector crush)
                  (setf (ewd-sector-floor-height-fx sector) last-pos)
                  (ese--change-sector sector crush)))
            (cl-decf (ewd-sector-floor-height-fx sector) speed)
            (when (ese--change-sector sector crush)
              (setf (ewd-sector-floor-height-fx sector) last-pos
                    res :crushed)
              (ese--change-sector sector crush))))
         (:up
          (if (> (+ (ewd-sector-floor-height-fx sector) speed) dest)
              (progn
                (setf (ewd-sector-floor-height-fx sector) dest
                      res :past-destination)
                (when (ese--change-sector sector crush)
                  (setf (ewd-sector-floor-height-fx sector) last-pos)
                  (ese--change-sector sector crush)))
            (cl-incf (ewd-sector-floor-height-fx sector) speed)
            (when (ese--change-sector sector crush)
              (setf res :crushed)
              (when (not crush)
                (setf (ewd-sector-floor-height-fx sector) last-pos)
                (ese--change-sector sector crush))))))
       res))
    (:ceiling ;; 1
     (let ((last-pos (ewd-sector-ceiling-height-fx sector))
           (res :ok))
       (cl-case direction
         (:down
          (if (< (- (ewd-sector-ceiling-height-fx sector) speed) dest)
              (progn
                (setf (ewd-sector-ceiling-height-fx sector) dest
                      res :past-destination)
                (when (ese--change-sector sector crush)
                  (setf (ewd-sector-ceiling-height-fx sector) last-pos)
                  (ese--change-sector sector crush)))
            (cl-decf (ewd-sector-ceiling-height-fx sector) speed)
            (when (ese--change-sector sector crush)
              (setf res :crushed)
              (when (not crush)
                (setf (ewd-sector-ceiling-height-fx sector) last-pos)
                (ese--change-sector sector crush)))))
         (:up
          (if (> (+ (ewd-sector-ceiling-height-fx sector) speed) dest)
              (progn
                (setf (ewd-sector-ceiling-height-fx sector) dest
                      res :past-destination)
                (when (ese--change-sector sector crush)
                  (setf (ewd-sector-ceiling-height-fx sector) last-pos)
                  (ese--change-sector sector crush)))
            (cl-incf (ewd-sector-ceiling-height-fx sector) speed)
            (ese--change-sector sector crush))))
       res))))

(defun ese--vertical-door-tick (door)
  "T_VerticalDoor in p_doors.c"
  (cl-ecase (ese--door-direction door)
    (:wait ;; 0
     (when (zerop (cl-decf (ese--door-top-count-down door)))
       (cl-case (ese--door-special-type door)
         ((:blaze-raise :normal)
          (setf (ese--door-direction door) :down))
         (:close-30-then-open
          (setf (ese--door-direction door) :up)))))
    (:initial-wait ;; 2
     (when (and (zerop (cl-decf (ese--door-top-count-down door)))
                (eq (ese--door-type door) :raise-in-5-minutes))
       (setf (ese--door-direction door) :up)
       (setf (ese--door-type door) :normal)))
    (:down ;; -1
     (let* ((sector (ese--door-sector door))
            (res (ese--move-plane sector (ese--door-speed door) (ewd-sector-floor-height-fx sector)
                                  nil :ceiling (ese--door-direction door))))
       (cl-case res
         (:past-destination
          (cl-case (ese--door-type door)
            ((:blaze-raise :blaze-close :normal :close)
             (setf (ewd-sector-special-data sector) nil)
             (eth-thinker-remove (ese--door-thinker door)))
            ((:close-30-then-open)
             (setf (ese--door-direction door) :wait
                   (ese--door-top-count-down door) (* 35 30)))))  ;; TODO: extract to constant
         (:crushed
          (cl-case (ese--door-type door)
            ((:blaze-close :close) nil)
            (t
             (setf (ese--door-direction door) :up)))))))
    (:up ;; 1
     (let* ((sector (ese--door-sector door))
            (res (ese--move-plane sector (ese--door-speed door) (ese--door-top-height door)
                                  nil :ceiling (ese--door-direction door))))
       (when (eq res :past-destination)
         (cl-case (ese--door-type door)
           ((:blaze-raise :normal)
            (setf (ese--door-direction door) :wait
                  (ese--door-top-count-down door) (ese--door-top-wait door)))
           ((:close-30-then-open :blaze-raise :open)
            (setf (ewd-sector-special-data sector) nil)
            (eth-thinker-remove (ese--door-thinker door)))))))))

(defun ese-vertical-door (linedef mobj)
  "EV_VerticalDoor in p_doors.c"
  (let* ((special (ewd-linedef-special linedef))
         (sector (ewd-linedef-back-sector linedef))
         (special-data (ewd-sector-special-data sector))
         (player (emo-player mobj))
         (cards (when player (emo-player-cards player)))
         (can-open t))
    (cond
     ((or (= special ese-lt-blue-door)
          (= special ese-lt-blue-door-stay-open))
      (when (not player)
        (setq can-open nil))
      (when (and (not (memq :blue-card cards)) (not (memq :blue-skull cards)))
        (setf can-open nil
              (emo-player-message player) "You need blue key to open this door")))
     ((or (= special ese-lt-yellow-door)
          (= special ese-lt-yellow-door-stay-open))
      (when (not player)
        (setq can-open nil))
      (when (and (not (memq :yellow-card cards)) (not (memq :yellow-skull cards)))
        (setf can-open nil
              (emo-player-message player) "You need yellow key to open this door")))
     ((or (= special ese-lt-red-door)
          (= special ese-lt-red-door-stay-open))
      (when (not player)
        (setq can-open nil))
      (when (and (not (memq :red-card cards)) (not (memq :red-skull cards)))
        (setf can-open nil
              (emo-player-message player) "You need red key to open this door"))))
    (when can-open
      (if (and special-data
               (or (emo-player mobj)
                   (ese--can-monster-open-closing-p special)))
          ;; Sector has already been activated
          (if (eq :down (ese--door-direction special-data))
              (setf (ese--door-direction special-data) :up)
            (when (emo-player mobj)
              (setf (ese--door-direction special-data) :down)))
        (let* ((door (ese--make-door :sector sector :type (ese--door-special-type special)
                                     :direction :up
                                     :top-wait ese-vertical-door-wait
                                     :top-height (ese--door-special-top-height special sector)
                                     :speed (ese--door-special-speed special)))
               (thinker (eth-register door #'ese--vertical-door-tick)))
          (setf (ese--door-thinker door) thinker
                (ewd-sector-special-data sector) door))))))

(defun ese-spawn-door-close-in-30 (sector)
  "P_SpawnDoorCloseIn30 in p_doors.c"
  (let* ((door (ese--make-door :sector sector :type :normal
                               :direction :wait
                               :top-count-down (* 30 eldoom-globals-tic-rate)
                               :speed ese--vertical-door-speed))
         (thinker (eth-register door #'ese--vertical-door-tick)))
    (setf (ese--door-thinker door) thinker
          (ewd-sector-special-data sector) door
          (ewd-sector-special sector) 0)))

(defun ese-spawn-door-raise-in-5-minutes (sector)
  "P_SpawnDoorRaiseIn5Mins in p_doors.c"
  (let* ((door (ese--make-door :sector sector :type :raise-in-5-minutes
                               :direction :initial-wait
                               :speed ese--vertical-door-speed
                               :top-height (- (emp-lowest-ceiling-surrounding-sector sector)
                                              (eldoom-math-fixed-from-int 4))
                               :top-wait ese-vertical-door-wait
                               :top-count-down (* 5 60 eldoom-globals-tic-rate)))
         (thinker (eth-register door #'ese--vertical-door-tick)))
    (setf (ese--door-thinker door) thinker
          (ewd-sector-special-data sector) door
          (ewd-sector-special sector) 0)))

(cl-defstruct (ese--button (:constructor ese--make-button) (:copier nil))
  sidedef
  pos
  texture
  (time 0 :type fixnum)
  next)
(defvar-local ese--buttons nil)
(define-eldoom-intrusive-freelist eldoom-sector--button
                                  eldoom-sector--button-next
                                  eldoom-sector--make-button)

(defun ese--remove-button (button)
  (setf (ese--button-sidedef button) nil
        (ese--button-texture button) nil)
  (ese--button-move-to-freelist button))

(defun ese--update-buttons ()
  (let ((button ese--buttons))
    (while button
      (cl-decf (ese--button-time button))
      (if (zerop (ese--button-time button))
          (progn
            (cl-ecase (ese--button-pos button)
              (:upper (setf (ewd-sidedef-upper (ese--button-sidedef button))
                            (ese--button-texture button)))
              (:middle (setf (ewd-sidedef-middle (ese--button-sidedef button))
                             (ese--button-texture button)))
              (:lower (setf (ewd-sidedef-lower (ese--button-sidedef button))
                            (ese--button-texture button))))
            (let ((next-button (ese--button-next button)))
              (ese--remove-button button)
              (setq button next-button)))
        (setq button (ese--button-next button))))))

(defun ese--start-button (sidedef pos texture time)
  "P_StartButton in p_switch.c"
  (catch :return
    (let ((button ese--buttons))
      (while button
        (when (and (> (ese--button-time button) 0)
                   (eq (ese--button-sidedef button) sidedef))
          (throw :return t))
        (setq button (ese--button-next button)))) ;; already pressed
    (let ((button (ese--button-new-from-freelist)))
      (setf (ese--button-sidedef button) sidedef
            (ese--button-pos button) pos
            (ese--button-texture button) texture
            (ese--button-time button) time
            (ese--button-next button) ese--buttons
            ese--buttons button))))

(defconst ese--button-time 35)

(defun ese-change-switch-texture (linedef use-again)
  "P_ChangeSwitchTexture in p_switch.c"
  (when (not use-again)
    (setf (ewd-linedef-special linedef) 0
          (ewd-linedef-on-use linedef) nil))
  (let* ((side (ewd-linedef-front-sidedef linedef)))
    (when-let* ((upper (ewd-sidedef-upper side))
                (upper-switched (ewd-texture-switched upper)))
      (setf (ewd-sidedef-upper side) upper-switched)
      (when use-again
        (ese--start-button side :upper upper ese--button-time)))
    (when-let* ((middle (ewd-sidedef-middle side))
                (middle-switched (ewd-texture-switched middle)))
      (setf (ewd-sidedef-middle side) middle-switched)
      (when use-again
        (ese--start-button side :middle middle ese--button-time)))
    (when-let* ((lower (ewd-sidedef-lower side))
                (lower-switched (ewd-texture-switched lower)))
      (setf (ewd-sidedef-lower side) lower-switched)
      (when use-again
        (ese--start-button side :lower lower ese--button-time)))))

(defun ese-do-donut (linedef)
  "EV_DoDonut in p_spec.c"
  (let ((res nil))
    (cl-loop for sector across (ewd-linedef-tagged-sectors linedef) do
             (when (not (ewd-sector-special-data sector))
               (setq res t)
               (let ((other-sector (emp-next-sector (aref (ewd-sector-linedefs sector) 0)
                                                    sector)))
                 (catch :break
                   (cl-loop for sec-line across (ewd-sector-linedefs other-sector) do
                            (when (and (eldoom-math-has-flag (ewd-linedef-flags sec-line)
                                                             enf-ML_TWOSIDED)
                                       (not (eq (ewd-linedef-back-sector sec-line) sector)))
                              (let* ((target-sec (ewd-linedef-back-sector sec-line))
                                     (dest-height (ewd-sector-floor-height-fx target-sec))
                                     (rising-floor
                                      (ese--make-floor :type :donut-raise 
                                                       :sector other-sector 
                                                       :direction :up
                                                       :dest-height dest-height
                                                       :speed (/ ese--floor-speed 2)
                                                       :texture (ewd-sector-floor-texture target-sec)))
                                     (rising-floor-thinker (eth-register rising-floor #'ese--move-floor-tick))
                                     (lowering-floor
                                      (ese--make-floor :type :lower-floor
                                                       :sector sector
                                                       :direction :down
                                                       :dest-height dest-height
                                                       :speed (/ ese--floor-speed 2)))
                                     (lowering-floor-thinker (eth-register lowering-floor #'ese--move-floor-tick)))
                                (setf (ese--floor-thinker rising-floor) rising-floor-thinker
                                      (ese--floor-thinker lowering-floor) lowering-floor-thinker
                                      (ewd-sector-special-data other-sector) rising-floor
                                      (ewd-sector-special-data sector) lowering-floor)
                                (throw :break t))))))))
    res))

(defun ese--plat-raise (plat)
  "T_PlatRaise in p_plats.c"
  (cl-ecase (ese--plat-direction plat)
    (:up
     (cl-case (ese--move-plane (ese--plat-sector plat)
                               (ese--plat-speed plat)
                               (ese--plat-top-height plat)
                               nil :floor :up)
       (:crushed
        (setf (ese--plat-count plat) (ese--plat-wait plat)
              (ese--plat-direction plat) :down))
       (:past-destination
        (setf (ese--plat-count plat) (ese--plat-wait plat)
              (ese--plat-direction plat) :wait)
        (when (not (eq (ese--plat-type plat) :perpetual-raise))
          (ese--remove-active-plat plat)))))
    (:down
     (when (eq (ese--move-plane (ese--plat-sector plat)
                                (ese--plat-speed plat)
                                (ese--plat-bottom-height plat)
                                nil :floor :down)
               :past-destination)
       (setf (ese--plat-count plat) (ese--plat-wait plat)
             (ese--plat-direction plat) :wait)))
    (:wait
     (cl-decf (ese--plat-count plat))
     (when (zerop (ese--plat-count plat))
       (if (= (ese--plat-bottom-height plat)
              (ewd-sector-floor-height-fx (ese--plat-sector plat)))
           (setf (ese--plat-direction plat) :up)
         (setf (ese--plat-direction plat) :down))))
    (:in-stasis nil)))

(defun ese--add-active-plat (plat)
  "P_AddActivePlat in p_plats.c"
  (if ese--active-plats
      (ese--plat-insert-before ese--active-plats plat)
    (setq ese--active-plats plat)))

(defun ese--remove-active-plat (plat)
  "P_RemoveActivePlat in p_plat.c"
  (eth-thinker-remove (ese--plat-thinker plat))
  (setf (ewd-sector-special-data (ese--plat-sector plat)) nil)
  (ese--remove-plat plat))

(defun ese--activate-in-stasis-plat (tag)
  "P_ActivateInStasisPlat in p_plats.c"
  (let ((plat ese--active-plats))
    (while plat
      (when (and (= (ese--plat-tag plat) tag)
                 (eq (ese--plat-direction plat) :in-stasis))
        (setf (ese--plat-direction plat) (ese--plat-old-direction plat)
              (eth-thinker-action (ese--plat-thinker plat)) #'ese--plat-raise))
      (setq plat (ese--plat-next plat)))))

(defun ese-do-plat (linedef type amount)
  "EV_DoPlay in p_plats.c"
  (let ((res nil))
    (when (eq type :perpetual-raise)
      (ese--activate-in-stasis-plat (ewd-linedef-sector-tag linedef)))
    (cl-loop for sector across (ewd-linedef-tagged-sectors linedef) do
             (when (not (ewd-sector-special-data sector))
               (setq res t)
               (let* ((plat (ese--plat-new-from-freelist))
                      (thinker (eth-register plat #'ese--plat-raise)))
                 (setf (ese--plat-type plat) type
                       (ese--plat-sector plat) sector 
                       (ese--plat-top-height plat)
                       (ese--plat-type-top-height type amount sector)
                       (ese--plat-bottom-height plat)
                       (ese--plat-type-bottom-height type sector)
                       (ese--plat-speed plat) (ese--plat-type-speed type)
                       (ese--plat-direction plat) (ese--plat-type-direction type)
                       (ese--plat-wait plat) (ese--plat-type-wait type)
                       (ese--plat-thinker plat) thinker)
                 (when (or (eq type :raise-to-nearest-and-change)
                           (eq type :raise-and-change))
                   (setf (ewd-sector-floor-texture sector)
                         (ewd-sector-floor-texture (ewd-sidedef-sector (ewd-linedef-front-sidedef linedef)))))
                 (ese--add-active-plat plat))))
    res))

(defun ese--plat-nop (_plat)
  "Null action for plat in stasis")

(defun ese-stop-plat (linedef)
  "EV_StopPlat in p_plats.c"
  (let ((plat ese--active-plats))
    (while plat
      (when (and (= (ese--plat-tag plat) (ewd-linedef-sector-tag linedef))
                 (not (eq (ese--plat-direction plat) :in-stasis)))
        (setf (ese--plat-old-direction plat) (ese--plat-direction plat)
              (eth-thinker-action (ese--plat-thinker plat)) #'ese--plat-nop
              (ese--plat-direction plat) :in-stasis)))))

(defun ese--move-floor-tick (floor)
  "T_MoveFloor in p_floor.c"
  (when (eq :past-destination (ese--move-plane (ese--floor-sector floor)
                                               (ese--floor-speed floor)
                                               (ese--floor-dest-height floor)
                                               (ese--floor-crush floor)
                                               :floor
                                               (ese--floor-direction floor)))
    (let ((sector (ese--floor-sector floor)))
      (setf (ewd-sector-special-data sector) nil)
      (cl-case (ese--floor-direction floor)
        (:up
         (when (eq (ese--floor-type floor) :donut-raise)
           (setf (ewd-sector-special sector) (ese--floor-new-special floor)
                 (ewd-sector-floor-texture sector) (ese--floor-texture floor))))
        (:down
         (when (eq (ese--floor-type floor) :lower-and-change)
           (setf (ewd-sector-special sector) (ese--floor-new-special floor)
                 (ewd-sector-floor-texture sector) (ese--floor-texture floor)))))
      (eth-thinker-remove (ese--floor-thinker floor)))))

(defun ese-do-floor (linedef type)
  "EV_DoFloor in p_floor.c"
  (let ((res nil))
    (cl-loop for sector across (ewd-linedef-tagged-sectors linedef) do
             (when (not (ewd-sector-special-data sector))
               (setq res t)
               (let* ((floor (ese--make-floor :type type :sector sector 
                                              :dest-height (ese--floor-type-dest-height type sector)
                                              :speed (ese--floor-type-speed type)
                                              :direction (ese--floor-type-direction type)))
                      (thinker (eth-register floor #'ese--move-floor-tick)))
                 (setf (ese--floor-thinker floor) thinker
                       (ewd-sector-special-data sector) floor)
                 (cl-case type
                   (:raise-floor-crush
                    (setf (ese--floor-crush floor) t))
                   (:raise-floor-24-and-change
                    (setf (ewd-sector-floor-texture sector) (ewd-sector-floor-texture (ewd-linedef-front-sector linedef))
                          (ewd-sector-special sector) (ewd-sector-special (ewd-linedef-front-sector linedef))))
                   (:lower-and-change
                    (setf (ese--floor-texture floor) (ewd-sector-floor-texture sector))
                    (cl-loop for linedef across (ewd-sector-linedefs sector) do
                             (when (eldoom-math-has-flag (ewd-linedef-flags linedef) enf-ML_TWOSIDED)
                               (let ((other-sector (emp-next-sector linedef sector)))
                                 (when (= (ewd-sector-floor-height-fx other-sector)
                                          (ese--floor-dest-height floor))
                                   (setf (ese--floor-texture floor)
                                         (ewd-sector-floor-texture other-sector)
                                         (ese--floor-new-special floor)
                                         (ewd-sector-special other-sector)))))))))))
    res))

(defun ese--add-active-ceiling (ceiling)
  "P_AddActiveCeiling in p_ceilng.c"
  (if ese--active-ceilings
      (ese--ceiling-insert-before ese--active-ceilings ceiling)
    (setq ese--active-ceilings ceiling)))

(defun ese--remove-active-ceiling (ceiling)
  "P_RemoveActiveCeiling in p_ceilng.c"
  (eth-thinker-remove (ese--ceiling-thinker ceiling))
  (setf (ewd-sector-special-data (ese--ceiling-sector ceiling)) nil)
  (ese--remove-ceiling ceiling))

(defun ese--ceiling-nop (_linedef)
  "Null action for ceilings in stasis")

(defun ese-ceiling-crush-stop (linedef)
  "EV_CeilingCrushStop in p_ceilng.c"
  (let ((ceiling ese--active-ceilings)
        (res nil))
    (while ceiling
      (when (and (= (ese--ceiling-tag ceiling) (ewd-linedef-sector-tag linedef))
                 (not (eq (ese--ceiling-direction ceiling) :in-stasis)))
        (setf (ese--ceiling-old-direction ceiling) (ese--ceiling-direction ceiling)
              (eth-thinker-action (ese--ceiling-thinker ceiling)) #'ese--ceiling-nop
              (ese--ceiling-direction ceiling) :in-stasis
              res t)))
    res))      

(defun ese--move-ceiling-tick (ceiling)
  "T_MoveCeiling in p_ceilng.c"
  (cl-ecase (ese--ceiling-direction ceiling)
    (:in-stasis nil)
    (:up
     (when (eq (ese--move-plane (ese--ceiling-sector ceiling)
                                (ese--ceiling-speed ceiling)
                                (ese--ceiling-top-height ceiling)
                                nil :ceiling :up)
               :past-destination)
       (cl-case (ese--ceiling-type ceiling)
         (:raise-to-highest
          (ese--remove-active-ceiling ceiling))
         ((:silent-crush-and-raise :fast-crush-and-raise :crush-and-raise)
          (setf (ese--ceiling-direction ceiling) :down)))))
    (:down
     (cl-case (ese--move-plane (ese--ceiling-sector ceiling)
                               (ese--ceiling-speed ceiling)
                               (ese--ceiling-bottom-height ceiling)
                               (ese--ceiling-crush ceiling) :ceiling
                               :down)
       (:past-destination
        (cl-case (ese--ceiling-type ceiling)
          ((:silent-crush-and-raise :crush-and-raise)
           (setf (ese--ceiling-speed ceiling) ese--ceiling-speed
                 (ese--ceiling-direction ceiling) :up))
          (:fast-crush-and-raise
           (setf (ese--ceiling-direction ceiling) :up))
          ((:lower-and-crush :lower-to-floor)
           (ese--remove-active-ceiling ceiling))))
       (:crushed
        (cl-case (ese--ceiling-type ceiling)
          ((:silent-crush-and-raise :crush-and-raise :lower-and-crush)
           (setf (ese--ceiling-speed ceiling) (/ ese--ceiling-speed 8)))))))))


(defun ese--activate-in-stasis-ceiling (linedef)
  "P_ActivateInStasisCeiling in p_ceilng.c"
  (let ((ceiling ese--active-ceilings))
    (while ceiling
      (when (and (= (ese--ceiling-tag ceiling) (ewd-linedef-sector-tag linedef))
                 (eq (ese--ceiling-direction ceiling) :in-stasis))
        (setf (ese--ceiling-direction ceiling) (ese--ceiling-old-direction ceiling)
              (eth-thinker-action (ese--ceiling-thinker ceiling)) #'ese--move-ceiling-tick))
      (setq ceiling (ese--ceiling-next ceiling)))))

(defun ese-do-ceiling (linedef type)
  "EV_DoCeiling in p_ceilng.c"
  (let ((res nil))
    (when (or (eq type :fast-crush-and-raise)
              (eq type :silent-crush-and-raise)
              (eq type :crush-and-raise))
      (ese--activate-in-stasis-ceiling linedef))
    (cl-loop for sector across (ewd-linedef-tagged-sectors linedef) do
             (when (not (ewd-sector-special-data sector))
               (setq res t)
               (let* ((ceiling (ese--ceiling-new-from-freelist))
                      (thinker (eth-register ceiling #'ese--move-ceiling-tick)))
                 (setf (ese--ceiling-type ceiling) type
                       (ese--ceiling-sector ceiling) sector 
                       (ese--ceiling-top-height ceiling)
                       (ese--ceiling-type-top-height type sector)
                       (ese--ceiling-bottom-height ceiling)
                       (ese--ceiling-type-bottom-height type sector)
                       (ese--ceiling-speed ceiling) (ese--ceiling-type-speed type)
                       (ese--ceiling-direction ceiling)
                       (ese--ceiling-type-direction type)
                       (ese--ceiling-tag ceiling) (ewd-sector-tag sector)
                       (ese--ceiling-crush ceiling)
                       (or (eq type :fast-crush-and-raise)
                           (eq type :silent-crush-and-raise)
                           (eq type :crush-and-raise))
                       (ese--ceiling-thinker ceiling) thinker
                       (ewd-sector-special-data sector) ceiling)
                 (ese--add-active-ceiling ceiling))))
    res))

(defun ese-do-door (linedef type)
  "EV_DoDoor in p_doors.c"
  (let ((res nil))
    (cl-loop for sector across (ewd-linedef-tagged-sectors linedef) do
             (when (not (ewd-sector-special-data sector))
               (setq res t)
               (let* ((door (ese--make-door :sector sector :type type
                                            :top-wait ese-vertical-door-wait
                                            :top-height (ese--door-type-top-height type sector)
                                            :speed (ese--door-type-speed type)
                                            :direction (ese--door-type-direction type)))
                      (thinker (eth-register door #'ese--vertical-door-tick)))
                 (setf (ese--door-thinker door) thinker
                       (ewd-sector-special-data sector) door))))
    res))

(defun ese--special-key-color (special)
  (cl-case special
    ((99 133) :blue)
    ((134 135) :red)
    ((136 137) :yellow)))

(defun ese-do-locked-door (linedef type mobj)
  "EV_DoLockedDoor in p_doors.c"
  (when-let* ((player (emo-player mobj))
              (cards (emo-player-cards player)))
    (catch :no-key
      (cl-ecase (ese--special-key-color (ewd-linedef-special linedef))
        (:blue
         (when (and (not (memq :blue-card cards))
                    (not (memq :blue-skull cards)))
           (setf (emo-player-message player) "You need a blue key to activate this object")
           (throw :no-key nil)))
        (:red
         (when (and (not (memq :red-card cards))
                    (not (memq :red-skull cards)))
           (setf (emo-player-message player) "You need a red key to activate this object")
           (throw :no-key nil)))
        (:yellow
         (when (and (not (memq :yellow-card cards))
                    (not (memq :yellow-skull cards)))
           (setf (emo-player-message player) "You need a yellow key to activate this object")
           (throw :no-key nil))))
      (ese-do-door linedef type))))

(defun ese-build-stairs (linedef type)
  "EV_BuildStairs in p_floor.c"
  (let ((res nil))
    (cl-loop for sector across (ewd-linedef-tagged-sectors linedef) do
             (when (not (ewd-sector-special-data sector))
               (setq res t)
               (let* ((stair-size (cl-ecase type
                                    (:build-8 (eldoom-math-fixed-from-int 8))
                                    (:turbo-16 (eldoom-math-fixed-from-int 16))))
                      (height (+ (ewd-sector-floor-height-fx sector) stair-size))
                      (speed (cl-ecase type
                               (:build-8 (/ ese--floor-speed 4))
                               (:turbo-16 (* 4 ese--floor-speed))))
                      (floor (ese--make-floor :type type :sector sector 
                                              :direction :up
                                              :speed speed
                                              :dest-height height))
                      (thinker (eth-register floor #'ese--move-floor-tick))
                      (ok t))
                 (setf (ese--floor-thinker floor) thinker)
	         ;; Find next sector to raise
	         ;; 1.	Find 2-sided line with same sector front side and texture
	         ;; 2.	Other side is the next sector to raise
                 (while ok
                   (setq ok nil)
                   (catch :break
                     (cl-loop for sec-line across (ewd-sector-linedefs sector) do
                              (when (eldoom-math-has-flag (ewd-linedef-flags linedef)
                                                          enf-ML_TWOSIDED)
                                (let ((front-sec (ewd-linedef-front-sector sec-line))
                                      (back-sec (ewd-linedef-back-sector sec-line)))
                                  (when (and (eq front-sec sector)
                                             (eq (ewd-sector-floor-texture back-sec)
                                                 (ewd-sector-floor-texture sector)))
                                    (cl-incf height stair-size)
                                    (when (not (ewd-sector-special-data back-sec))
                                      (let* ((floor (ese--make-floor
                                                     :type type :sector back-sec
                                                     :direction :up
                                                     :speed speed
                                                     :dest-height height))
                                             (thinker (eth-register floor #'ese--move-floor-tick)))
                                        (setf (ese--floor-thinker floor) thinker
                                              (ewd-sector-special-data back-sec) floor
                                              sector back-sec
                                              ok t))))))))))))
    res))

(defun ese-light-turn-on (linedef brightness)
  "EV_LightTurnOn in p_lights.c"
  (cl-loop for sector across (ewd-linedef-tagged-sectors linedef) do
	   ;; brightness = 0 means to search for highest light level surrounding sector
           (when (zerop brightness)
             (cl-loop for sector-line across (ewd-sector-linedefs sector) do
                      (when-let* ((other-sector (emp-next-sector sector-line sector)))
                        (setq brightness (max brightness (ewd-sector-light other-sector))))))
           (setf (ewd-sector-light sector) brightness)))

(defun ese-turn-tag-lights-off (linedef)
  "EV_TurnTagLightsOff"
  (cl-loop for sector across (ewd-linedef-tagged-sectors linedef) do
           (let ((min-light (ewd-sector-light sector)))
             (cl-loop for sector-line across (ewd-sector-linedefs sector) do
                      (when-let* ((other-sector (emp-next-sector sector-line sector)))
                        (setq min-light (min min-light (ewd-sector-light other-sector)))))
             (setf (ewd-sector-light sector) min-light))))

(defun ese--find-teleport-destination (thinker sector)
  (let ((mobj (eth-thinker-object thinker)))
    (when (and (emo-p mobj)
               (= (emo-type mobj) enf-MT_TELEPORTMAN)
               (eq (ewd-subsector-sector (emo-subsector mobj)) sector))
      (throw :teleport mobj))))

(defun ese-teleport (linedef from-side mobj)
  "EV_Teleport in p_telept.c"
  (catch :return
    (when (and from-side
               (not (eldoom-math-has-flag (emo-flags mobj) enf-MF_MISSILE)))
      (cl-loop for sector across (ewd-linedef-tagged-sectors linedef) do
               (when-let* ((teleport-thing (catch :teleport 
                                             (eth-for-each-thinker #'ese--find-teleport-destination sector))))
                 (let ((old-x (emo-x-fx mobj))
                       (old-y (emo-y-fx mobj))
                       (old-z (emo-z-fx mobj)))
                   (when (not (eph-teleport-move mobj (emo-x-fx teleport-thing) (emo-y-fx teleport-thing)))
                     (throw :return nil))
                   (setf (emo-z-fx mobj) (emo-floor-z mobj))
                   (when-let* ((player (emo-player mobj)))
                     (setf (emo-player-view-z player) (+ (emo-z-fx mobj) (emo-player-view-height player)))
                     (emo-spawn-one old-x old-y old-z enf-MT_TFOG)
                     (emo-spawn-one (+ (emo-x-fx teleport-thing) 
                                       (* 20 (eldoom-math-cos (emo-angle teleport-thing))))
                                    (+ (emo-y-fx teleport-thing) 
                                       (* 20 (eldoom-math-sin (emo-angle teleport-thing))))
                                    (emo-z-fx mobj) enf-MT_TFOG)
                     (when (emo-player mobj)
                       (setf (emo-reaction-time mobj) 18))
                     (setf (emo-angle mobj) (emo-angle teleport-thing)
                           (emo-momx mobj) 0
                           (emo-momy mobj) 0
                           (emo-momz mobj) 0)
                     (throw :return t))))))))

(cl-defstruct (ese--strobe (:constructor ese--make-strobe) (:copier nil))
  sector
  (dark-time 0 :type fixnum)
  (bright-time 0 :type fixnum)
  (max-light 0 :type fixnum)
  (min-light 0 :type fixnum)
  (count 0 :type fixnum))

(defun ese--strobe-flash (strobe)
  "T_StrobeFlash in p_lights.c"
  (cl-decf (ese--strobe-count strobe))
  (when (zerop (ese--strobe-count strobe))
    (let ((sector (ese--strobe-sector strobe)))
      (if (= (ewd-sector-light sector) (ese--strobe-min-light strobe))
          (setf (ewd-sector-light sector) (ese--strobe-max-light strobe)
                (ese--strobe-count strobe) (ese--strobe-bright-time strobe))
        (setf (ewd-sector-light sector) (ese--strobe-min-light strobe)
              (ese--strobe-count strobe) (ese--strobe-dark-time strobe))))))

(defconst ese-strobe-fast-dark-time 15)
(defconst ese-strobe-slow-dark-time 35)
(defconst ese--strobe-bright-time 5)

(defun ese-spawn-strobe-flash (sector dark-time in-sync)
  "P_SpawnStrobeFlash in p_lights.c"
  (declare (ftype (function (ewd-sector fixnum boolean) t)))
  (let* ((strobe (ese--make-strobe :sector sector 
                                   :dark-time dark-time
                                   :bright-time ese--strobe-bright-time
                                   :max-light (ewd-sector-light sector)
                                   :min-light (emp-min-surrounding-light sector (ewd-sector-light sector))
                                   :count (if in-sync 0 (eldoom-random-dice 1 8 t)))))
    (eth-register strobe #'ese--strobe-flash))
  (setf (ewd-sector-special sector) 0))

(defun ese-start-light-strobing (linedef)
  "EV_StartLightStrobing"
  (cl-loop for sector across (ewd-linedef-tagged-sectors linedef) do
           (unless (ewd-sector-special-data sector)
             (ese-spawn-strobe-flash sector ese-strobe-slow-dark-time nil))))

(cl-defstruct (ese--glow (:constructor ese--make-glow) (:copier nil))
  sector
  (min-light 0 :type fixnum)
  (max-light 0 :type fixnum)
  direction)

(defconst ese--glow-speed 8)

(defun ese--glow (glow)
  "T_Glow in p_lights.c"
  (let ((sector (ese--glow-sector glow)))
    (cl-ecase (ese--glow-direction glow)
      (:down
       (cl-decf (ewd-sector-light sector) ese--glow-speed)
       (when (<= (ewd-sector-light sector) (ese--glow-min-light glow))
         (cl-incf (ewd-sector-light sector) ese--glow-speed)
         (setf (ese--glow-direction glow) :up)))
      (:up
       (cl-incf (ewd-sector-light sector) ese--glow-speed)
       (when (>= (ewd-sector-light sector) (ese--glow-max-light glow))
         (cl-decf (ewd-sector-light sector) ese--glow-speed)
         (setf (ese--glow-direction glow) :down))))))

(defun ese-spawn-glowing-light (sector)
  "P_SpawnGlowingLight in p_lights.c"  
  (let* ((glow (ese--make-glow :sector sector
                               :min-light (emp-min-surrounding-light sector (ewd-sector-light sector))
                               :max-light (ewd-sector-light sector)
                               :direction :down)))
    (eth-register glow #'ese--glow))
  (setf (ewd-sector-special sector) 0))

(cl-defstruct (ese--fire-flicker (:constructor ese--make-fire-flicker) (:copier nil))
  sector
  (count 0 :type fixnum)
  (min-light 0 :type fixnum)
  (max-light 0 :type fixnum))

(defun ese--fire-flicker (flicker)
  "T_FireFlicker in p_lights.c"
  (cl-decf (ese--fire-flicker-count flicker))
  (when (zerop (ese--fire-flicker-count flicker))
    (let ((amount (* 16 (logand (eldoom-random t) 3)))
          (sector (ese--fire-flicker-sector flicker)))
      (setf (ewd-sector-light sector)
            (if (< (- (ewd-sector-light sector) amount) (ese--fire-flicker-min-light flicker))
                (ese--fire-flicker-min-light flicker)
              (- (ese--fire-flicker-max-light flicker) amount))
            (ese--fire-flicker-count flicker) 4))))

(defun ese-spawn-fire-flicker (sector)
  "P_SpawnFireFlicker in p_lights.c"
  (let* ((flicker (ese--make-fire-flicker
                   :sector sector
                   :count 4
                   :min-light (+ 16 (emp-min-surrounding-light sector (ewd-sector-light sector)))
                   :max-light (ewd-sector-light sector))))
    (eth-register flicker #'ese--fire-flicker))
  (setf (ewd-sector-special sector) 0))

(cl-defstruct (ese--light-flash (:constructor ese--make-light-flash) (:copier nil))
  sector
  (count 0 :type fixnum)
  (max-light 0 :type fixnum)
  (min-light 0 :type fixnum)
  (max-time 0 :type fixnum)
  (min-time 0 :type fixnum))

(defun ese--light-flash (flash)
  "T_LightFlash in p_lights.c"
  (cl-decf (ese--light-flash-count flash))
  (let ((sector (ese--light-flash-sector flash))
        (min-light (ese--light-flash-min-light flash))
        (min-time (ese--light-flash-min-time flash))
        (max-light (ese--light-flash-max-light flash))
        (max-time (ese--light-flash-max-time flash)))
    (when (zerop (ese--light-flash-count flash))
      (if (= (ewd-sector-light sector) max-light)
          (setf (ewd-sector-light sector) min-light
                (ese--light-flash-count flash) (1+ (logand (eldoom-random t) min-time)))
        (setf (ewd-sector-light sector) max-light
              (ese--light-flash-count flash) (1+ (logand (eldoom-random t) max-time)))))))

(defun ese-spawn-light-flash (sector)
  "P_SpawnLightFlash in p_lights.c"
  (let* ((flash (ese--make-light-flash
                 :sector sector
                 :min-light (emp-min-surrounding-light sector (ewd-sector-light sector))
                 :max-light (ewd-sector-light sector)
                 :max-time 64
                 :min-time 7
                 :count (1+ (logand (eldoom-random t) 64)))))
    (eth-register flash #'ese--light-flash))
  (setf (ewd-sector-special sector) 0))

(defun ese-use-special-line (mobj linedef)
  "P_UseSpecialLine in p_switch.c"
  (when-let* ((on-use (ewd-linedef-on-use linedef)))
    (funcall on-use mobj linedef)))

(defun ese-update-specials ()
  (ese--update-buttons))

;; Local Variables:
;; read-symbol-shorthands: (("ese-" . "eldoom-sector-") ("eth-" . "eldoom-think-") ("emo-" . "eldoom-mobj-") ("eph-" . "eldoom-phys-") ("ewd-" . "eldoom-wad-") ("enf-" . "eldoom-info-") ("emp-" . "eldoom-map-"))
;; End:
(provide 'eldoom-sector)
;;; eldoom-sector.el ends here
