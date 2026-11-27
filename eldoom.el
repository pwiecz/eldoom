;;; eldoom.el --- Doom engine implemented in emacs lisp -*- lexical-binding:t -*-

;; Copyright (C) 2022 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Invoke (eldoom) to run Doom inside emacs

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'profiler)
(require 'seq)

(require 'eldoom-actor)
(require 'eldoom-canvas)
(require 'eldoom-config)
(require 'eldoom-globals)
(require 'eldoom-info)
(require 'eldoom-map)
(require 'eldoom-math)
(require 'eldoom-mobj)
(require 'eldoom-phys)
(require 'eldoom-random)
(require 'eldoom-render)
(require 'eldoom-special)
(require 'eldoom-think)
(require 'eldoom-trace)
(require 'eldoom-user)
(require 'eldoom-wad)

;; (byte-recompile-directory (file-name-directory (or load-file-name (buffer-file-name))) 0 t)

(defvar-local eldoom-player nil)

(defvar-local eldoom-canvas nil)
(defvar-local eldoom-quitting nil)
(defvar-local eldoom-frame-gc-cons-threshold (* 100 1024 1024))
;;   "Temporary `gc-cons-threshold' used while rendering animation frames.")
(defvar-local eldoom--saved-gc-cons-threshold nil)
(defvar-local eldoom--saved-mouse-color nil)
(defvar-local eldoom--saved-x-pointer-shape nil)
(defvar-local eldoom--tic-timer nil)

(defvar-local eldoom--mouse-preset-x nil)
(defvar-local eldoom--mouse-position-jitter 0 
  "Alters arguments to set-mouse-pixel-position to prevent emacs from ignoring
calls to the function")
(defvar-local eldoom--mouse-sensitivity 5000000)

(defun eldoom--mouse-move ()
  (interactive)
  ;; TODO: calling mouse-pixel-position on each mouse movement event is pretty expensive
  ;; and shows up prominently in the profiler output. Reading position from event coordinates
  ;; may be cheaper, but requires translating one to the other to take into account
  ;; window margins and such.
  (let* ((pos (mouse-pixel-position))
         (x (cadr pos))
         (y (cddr pos))
         (left (window-pixel-left))
         (top (window-pixel-top)))
    (when (and (<= 0 x (+ left (1- (window-pixel-width)))) (<= 0 y (+ (1- (window-pixel-height)) top)))
      (when eldoom--mouse-preset-x
        (let ((dx (- x eldoom--mouse-preset-x)))
          (eldoom-math-angle-decf (eldoom-user-command-angle-turn eldoom-user-cmd)
				  (* dx eldoom--mouse-sensitivity))))
      (setq eldoom--mouse-preset-x (+ left (/ eldoom-canvas-scaled-width 2)))
      (set-mouse-pixel-position (selected-frame)
                                eldoom--mouse-preset-x
                                (+ eldoom--mouse-position-jitter top (/ eldoom-canvas-scaled-height 2)))
      (setq eldoom--mouse-position-jitter (- 1 eldoom--mouse-position-jitter)))))

(defvar-local eldoom--mouse-left-down-p nil)
(defun eldoom--mouse-left-down ()
  (interactive)
  (setq eldoom--mouse-left-down-p t))

(defun eldoom--mouse-left-up ()
  (interactive)
  (setq eldoom--mouse-left-down-p nil))

(defun eldoom--fist ()
  (interactive)
  (setf (eldoom-user-command-weapon-change-button eldoom-user-cmd) eldoom-globals-wp_fist))
(defun eldoom--pistol ()
  (interactive)
  (setf (eldoom-user-command-weapon-change-button eldoom-user-cmd) eldoom-globals-wp_pistol))
(defun eldoom--shotgun ()
  (interactive)
  (setf (eldoom-user-command-weapon-change-button eldoom-user-cmd) eldoom-globals-wp_shotgun))
(defun eldoom--chaingun ()
  (interactive)
  (setf (eldoom-user-command-weapon-change-button eldoom-user-cmd) eldoom-globals-wp_chaingun))
(defun eldoom--rocket ()
  (interactive)
  (setf (eldoom-user-command-weapon-change-button eldoom-user-cmd) eldoom-globals-wp_missile))
(defun eldoom--plasma ()
  (interactive)
  (setf (eldoom-user-command-weapon-change-button eldoom-user-cmd) eldoom-globals-wp_plasma))
(defun eldoom--bfg ()
  (interactive)
  (setf (eldoom-user-command-weapon-change-button eldoom-user-cmd) eldoom-globals-wp_bfg))

(defun eldoom--handle-any-key ()
  (interactive))

(defvar eldoom-mode-map nil)
(setq eldoom-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "<left>") #'eldoom-turn-left-key)
        (define-key map (kbd "a") #'eldoom-strafe-left-key)
        (define-key map (kbd "A") #'eldoom-strafe-left-fast-key)
        (define-key map (kbd "S-<left>") #'eldoom-turn-left-fast-key)
        (define-key map (kbd "<right>") #'eldoom-turn-right-key)
        (define-key map (kbd "d") #'eldoom-strafe-right-key)
        (define-key map (kbd "D") #'eldoom-strafe-right-fast-key)
        (define-key map (kbd "S-<right>") #'eldoom-turn-right-fast-key)
        (define-key map (kbd "M-<left>") #'eldoom-strafe-left-key)
        (define-key map (kbd "M-S-<left>") #'eldoom-strafe-left-fast-key)
        (define-key map (kbd "M-<right>") #'eldoom-strafe-right-key)
        (define-key map (kbd "M-S-<right>") #'eldoom-strafe-right-fast-key)
        (define-key map (kbd "<up>") #'eldoom-move-forward-key)
        (define-key map (kbd "M-<up>") #'eldoom-move-forward-key)
        (define-key map (kbd "S-<up>") #'eldoom-move-forward-fast-key)
        (define-key map (kbd "M-S-<up>") #'eldoom-move-forward-fast-key)
        (define-key map (kbd "w") #'eldoom-move-forward-key)
        (define-key map (kbd "W") #'eldoom-move-forward-fast-key)
        (define-key map (kbd "<down>") #'eldoom-move-backward-key)
        (define-key map (kbd "M-<down>") #'eldoom-move-backward-key)
        (define-key map (kbd "S-<down>") #'eldoom-move-backward-fast-key)
        (define-key map (kbd "M-S-<down>") #'eldoom-move-backward-fast-key)
        (define-key map (kbd "s") #'eldoom-move-backward-key)
        (define-key map (kbd "S") #'eldoom-move-backward-fast-key)
        (define-key map (kbd "<SPC>") #'eldoom-use)
        (define-key map (kbd "M-<SPC>") #'eldoom-use)
        (define-key map (kbd "S-<SPC>") #'eldoom-use)
        (define-key map (kbd "M-S-<SPC>") #'eldoom-use)
        (define-key map (kbd "1") #'eldoom--fist)
        (define-key map (kbd "2") #'eldoom--pistol)
        (define-key map (kbd "3") #'eldoom--shotgun)
        (define-key map (kbd "4") #'eldoom--chaingun)
        (define-key map (kbd "5") #'eldoom--rocket)
        (define-key map (kbd "6") #'eldoom--plasma)
        (define-key map (kbd "7") #'eldoom--bfg)
        (define-key map (kbd "q") #'eldoom--keyboard-quit)
        (define-key map (kbd "C-g") #'eldoom--keyboard-quit)
        (define-key map (kbd "C-x") ctl-x-map)
        (define-key map [mouse-movement] #'eldoom--mouse-move)
        (define-key map [down-mouse-1] #'eldoom--mouse-left-down)
        (define-key map [double-down-mouse-1] #'eldoom--mouse-left-down)
        (define-key map [triple-down-mouse-1] #'eldoom--mouse-left-down)
        (define-key map [mouse-1] #'eldoom--mouse-left-up)
        (define-key map [double-mouse-1] #'eldoom--mouse-left-up)
        (define-key map [triple-mouse-1] #'eldoom--mouse-left-up)
        (define-key map [drag-mouse-1] #'eldoom--mouse-left-up)
        (define-key map [double-drag-mouse-1] #'eldoom--mouse-left-up)
        (define-key map [triple-drag-mouse-1] #'eldoom--mouse-left-up)
        (define-key map [t] #'eldoom--handle-any-key)
        map))

(defun eldoom--on-kill-buffer ()
  (eldoom-quit))

(define-derived-mode eldoom-mode special-mode "ELDooom"
  ;; Override local map to handle S+cursor key bindings, specified in windwmove-mode
  (setq-local overriding-local-map eldoom-mode-map
              track-mouse t
              mouse-fine-grained-tracking t
              cursor-type nil
              make-pointer-invisible t
              mouse-highlight nil
              buffer-read-only t)
  (buffer-disable-undo)
  (add-hook 'kill-buffer-hook #'eldoom--on-kill-buffer nil 'local))

(defvar x-pointer-shape)     ;; declare those variables to silcence warnings
(defvar x-pointer-invisible) ;; when compiling e.g. on Windows

(defun eldoom-init ()
  (setq debug-on-error t)
  (unless eldoom--saved-gc-cons-threshold
    (setq eldoom--saved-gc-cons-threshold gc-cons-threshold))
  (unless eldoom--saved-mouse-color
    (setq eldoom--saved-mouse-color (alist-get 'mouse-color (frame-parameters))))
  (when (eq (window-system) 'x)
    (unless eldoom--saved-x-pointer-shape
      (setq eldoom--saved-x-pointer-shape x-pointer-shape))
    (setq x-pointer-shape x-pointer-invisible))
  (if (and (stringp eldoom--saved-mouse-color) (string= eldoom--saved-mouse-color "yellow"))
      (set-mouse-color "#aaaaaa")
    (set-mouse-color "yellow")) ;; change mouse color to trigger applying new x-pointer-shape
  (setq gc-cons-threshold eldoom-frame-gc-cons-threshold)
  (when (or (< most-positive-fixnum 281474976710655)
            (> most-negative-fixnum -281474976710656))
    (error "Fixnum range is too small [%d, %d], at least 48-bits needed"
           most-negative-fixnum most-positive-fixnum))
  ;;(pop-to-buffer-same-window (get-buffer-create eldoom-globals-buffer-name))
  (let ((buf (get-buffer-create eldoom-globals-buffer-name)))
    (switch-to-buffer buf))
  (setq eldoom-quitting nil)
  (eldoom-mode)
  (setq eldoom-canvas (eldoom-create-canvas eldoom-canvas-width eldoom-canvas-height)))

(defun eldoom-quit ()
  (unless eldoom-quitting
    (message "Quitting eldoom")
    (setq eldoom-quitting t
          overriding-local-map nil)
    (when (timerp eldoom--tic-timer)
      (cancel-timer eldoom--tic-timer))
    (when eldoom--saved-gc-cons-threshold
      (setq gc-cons-threshold eldoom--saved-gc-cons-threshold
            eldoom--saved-gc-cons-threshold nil))
    (when (eq (window-system) 'x)
      (setq x-pointer-shape eldoom--saved-x-pointer-shape))
    (set-mouse-color eldoom--saved-mouse-color)
    (when (get-buffer eldoom-globals-buffer-name)
      (kill-buffer eldoom-globals-buffer-name)
      (clear-image-cache))
    (setq debug-on-error nil)))

(defun eldoom--keyboard-quit ()
  (interactive)
  (eldoom-quit))

(defvar eldoom-wad-path nil
  "Default WAD path used by ELDoom test helpers.")

(if (eq system-type 'windows-nt)
    ;;  (setq eldoom-wad-path "c:/wad/freedoom2.wad")
    (setq eldoom-wad-path "c:/wad/DOOM2.WAD")
  ;; (setq eldoom-wad-path "/usr/share/games/doom/freedoom2.wad"))
  ;; (setq eldoom-wad-path "/home/piotr/wad/doom.wad"))
  (setq eldoom-wad-path "/home/piotr/wad/DOOM2.WAD"))

(defun eldoom--init-player-at-thing (player-start)
  (setq eldoom-player (emo-spawn-player player-start))
  (eth-register-mobj-thinker (emo-player-mobj eldoom-player)))

(defun eldoom--game-buffer ()
  "Return the live ELDoom display buffer, or signal an error."
  (or (get-buffer eldoom-globals-buffer-name)
      (error "ELDoom buffer %S is not open" eldoom-globals-buffer-name)))

(defmacro eldoom--with-game-buffer (&rest body)
  "Evaluate BODY with the ELDoom buffer current."
  `(with-current-buffer (eldoom--game-buffer) ,@body))

(defun eldoom--game-buffer-selected-p ()
  "Return non-nil when the ELDoom buffer is shown in the selected window."
  (let ((game-buffer (get-buffer eldoom-globals-buffer-name)))
    (and game-buffer
         (eq (window-buffer (selected-window)) game-buffer))))

(defun eldoom--thing-angle-bam (thing)
  (* eldoom-math-ang45 (/ (ewd-thing-angle-deg thing) 45)))

(defun eldoom-test-bsp-benchmark
    (&optional frames warmup-frames)
  "Benchmark BSP rendering without timers, redisplay, or image flushing.
With an interactive prefix argument, prompt for FRAMES."
  (interactive
   (list
    (read-number "Frames: " 100)
    (read-number "Warmup frames: " 10)))
  (let* ((frames (or frames 100))
         (warmup-frames (or warmup-frames 10))
         (directory (ewd-read-directory eldoom-wad-path))
         (map (ewd-load-map eldoom-wad-path directory "MAP01"))
         (scene (eldoom-render-scene-for-map map))
         (player-start (seq-find (lambda (thing)
                                   (= (ewd-thing-type thing) 1))
                                 (ewd-map-things map)))
         (x-fx (ewd-thing-x-fx player-start))
         (y-fx (+ (ewd-thing-y-fx player-start) 256))
         (z-fx (ewd-sector-floor-height-fx (emp-sector-at x-fx y-fx)))
         (canvas (eldoom-create-canvas
                  eldoom-canvas-width eldoom-canvas-height))
         (started-at nil)
         (elapsed nil))
    (setq debug-on-error t
          eldoom-globals-level-time 0
          eldoom-globals-game-tic (random 256)
          ewd-current-map map)
    (eldoom-random-init)
    (enf-init (ewd-map-sprites map))
    (eth-init)
    (eldoom-special-init map)
    (emo-init-things map)
    (eldoom--init-player-at-thing player-start)
    (eac-init-actions enf-states)
    (eldoom-user-init eldoom-player enf-states)
    (etr-init)
    (eph-init)
    (dotimes (_ warmup-frames)
      (eldoom-user-update-player eldoom-player)
      (eth-run-thinkers)
      (eldoom-special-update)
      (eldoom-render-render-level
       ewd-current-map scene canvas x-fx y-fx z-fx
       (eldoom--thing-angle-bam player-start) eldoom-player)
      (cl-incf eldoom-globals-level-time)
      (cl-incf eldoom-globals-game-tic))
    (garbage-collect)
    (setq started-at (float-time))
    (dotimes (_ frames)
      (eldoom-user-update-player eldoom-player)
      (eth-run-thinkers)
      (eldoom-special-update)
      (eldoom-render-render-level
       ewd-current-map scene canvas x-fx y-fx z-fx
       (eldoom--thing-angle-bam player-start) eldoom-player)
      (cl-incf eldoom-globals-level-time)
      (cl-incf eldoom-globals-game-tic))
    (setq elapsed (- (float-time) started-at))
    (message
     "BSP render-only benchmark: %d frames in %.4fs, %.4fs/frame"
     frames elapsed (/ elapsed frames))
    elapsed))

(defun eldoom-test-bsp-profile
    (&optional frames profiler-type)
  "Profile BSP rendering without redisplay, or image flushing.

PROFILER-TYPE defaults to `cpu'.  Use `mem' for memory profiling."
  (interactive
   (list
    (read-number "Frames: " 100)
    (intern (completing-read "Profiler type: " '("cpu" "mem" "cpu+mem")
                             nil t nil nil "cpu"))))
  (let* ((frames (or frames 100))
         (profiler-type (or profiler-type 'cpu))
         (_ (message "Reading wad directory from %s" eldoom-wad-path))
         (directory (ewd-read-directory eldoom-wad-path))
         (map-name "MAP01")
         (_ (message "Reading map %s" map-name))
         (map (ewd-load-map eldoom-wad-path directory map-name))
         (scene (eldoom-render-scene-for-map map))
         (_ (message "Map read"))
         (player-start (seq-find (lambda (thing)
                                   (= (ewd-thing-type thing) 1))
                                 (ewd-map-things map)))
         (canvas (eldoom-create-canvas
                  eldoom-canvas-width eldoom-canvas-height)))
    (setq eldoom-globals-level-time 0
          eldoom-globals-game-tic (random 256)
          ewd-current-map map)
    (eldoom-random-init)
    (enf-init (ewd-map-sprites map))
    (eth-init)
    (eldoom-special-init map)
    (emo-init-things map)
    (eldoom--init-player-at-thing player-start)
    (eac-init-actions enf-states)
    (eldoom-user-init eldoom-player enf-states)
    (etr-init)
    (eph-init)
    (cl-incf (emo-x-fx (emo-player-mobj eldoom-player)) (eldoom-math-fixed-from-int 550))
    (cl-incf (emo-y-fx (emo-player-mobj eldoom-player)) (eldoom-math-fixed-from-int 300))
    (eldoom-math-angle-incf (emo-angle (emo-player-mobj eldoom-player)) eldoom-math-ang90)
    (dotimes (_ 10)
      (eldoom-user-update-player eldoom-player)
      (eth-run-thinkers)
      (eldoom-special-update)
      (let ((player-mobj (emo-player-mobj eldoom-player)))
        (eldoom-render-render-level map scene canvas
                                    (emo-x-fx player-mobj) (emo-y-fx player-mobj)
                                    (emo-z-fx player-mobj) (emo-angle player-mobj)
                                    eldoom-player))
      (cl-incf eldoom-globals-level-time)
      (cl-incf eldoom-globals-game-tic))
    (garbage-collect)
    (profiler-stop)
    (profiler-reset)
    (profiler-start profiler-type)
    (dotimes (_ frames)
      (eldoom-user-update-player eldoom-player)
      (eth-run-thinkers)
      (eldoom-special-update)
      (let ((player-mobj (emo-player-mobj eldoom-player)))
        (eldoom-render-render-level map scene canvas
                                    (emo-x-fx player-mobj) (emo-y-fx player-mobj)
                                    (emo-z-fx player-mobj) (emo-angle player-mobj)
                                    eldoom-player))
      (cl-incf eldoom-globals-level-time)
      (cl-incf eldoom-globals-game-tic))
    (profiler-stop)
    (profiler-report)
    (message "Profiled %d BSP render-only frames with %s profiler"
             frames profiler-type)))

(defun eldoom-test-bsp ()
  (interactive)
  (eldoom-init)
  (let* ((_ (message "Reading wad directory from %s" eldoom-wad-path))
         (directory (ewd-read-directory eldoom-wad-path))
         (map-name "MAP01")
         (_ (message "Reading map %s" map-name))
         (map (ewd-load-map eldoom-wad-path directory map-name))
         (scene (eldoom-render-scene-for-map map))
         (_ (message "Map read"))
         (player-start (seq-find (lambda (thing)
                                   (= (ewd-thing-type thing) 1))
                                 (ewd-map-things map)))
         (start-x-fx)
         (start-y-fx)
         (max-render-time 0))
    (setq eldoom-globals-level-time 0
          eldoom-globals-game-tic (random 256)
          ewd-current-map map)
    (eldoom-random-init)
    (enf-init (ewd-map-sprites map))
    (eth-init)
    (eldoom-special-init map)
    (emo-init-things map)
    (eldoom--init-player-at-thing player-start)
    (eac-init-actions enf-states)
    (eldoom-user-init eldoom-player enf-states)
    (etr-init)
    (eph-init)
    (let ((player-mobj (emo-player-mobj eldoom-player)))
      (cl-incf (emo-x-fx player-mobj) (eldoom-math-fixed-from-int 550))
      (cl-incf (emo-y-fx player-mobj) (eldoom-math-fixed-from-int 300))
      (eldoom-math-angle-incf (emo-angle player-mobj) eldoom-math-ang90)
      (setq start-x-fx (emo-x-fx player-mobj)
            start-y-fx (emo-y-fx player-mobj)))
    (garbage-collect)
    (condition-case nil
        (while (not eldoom-quitting)
          (let* ((player-mobj (emo-player-mobj eldoom-player))
                 (started-at (float-time))
                 (offset-fx (eldoom-math-fixed-from-float (* 96.0 (sin (/ eldoom-globals-level-time 24.0)))))
                 (angle (emo-angle player-mobj))
                 (x (+ start-x-fx (eldoom-math-mul offset-fx (eldoom-math-cos angle))))
                 (y (+ start-y-fx (eldoom-math-mul offset-fx (eldoom-math-sin angle))))
                 (z (ewd-sector-floor-height-fx (emp-sector-at x y))))
            (setf (emo-x-fx player-mobj) x
                  (emo-y-fx player-mobj) y
                  (emo-z-fx player-mobj) z
                  (emo-floor-z player-mobj) z)
            (eldoom-user-update-player eldoom-player)
            (eth-run-thinkers)
            (eldoom-special-update)
            (eldoom-render-render-level
             map scene eldoom-canvas
             (emo-x-fx player-mobj) (emo-y-fx player-mobj)
             (emo-player-view-z eldoom-player) (emo-angle player-mobj) eldoom-player)
            (setq max-render-time (max (- (float-time) started-at)))
            (when (= 0 (% eldoom-globals-level-time 20))
              (message "eldoom-test-bsp render: %.3fs" max-render-time)
              (setq max-render-time 0))
            (eldoom-render eldoom-canvas)
            (redisplay t))
          (cl-incf eldoom-globals-level-time)
          (cl-incf eldoom-globals-game-tic))
      (quit
       (eldoom-quit)))))

(defun eldoom-bsp-no-display ()
  (interactive)
  ;; (eldoom-init)
  (setq eldoom-canvas (eldoom-create-canvas eldoom-canvas-width eldoom-canvas-height))
  (let* ((_ (message "Reading wad directory from %s" eldoom-wad-path))
         (directory (ewd-read-directory eldoom-wad-path))
         (map-name "MAP01")
         (_ (message "Reading map %s" map-name))
         (map (ewd-load-map eldoom-wad-path directory map-name))
         (scene (eldoom-render-scene-for-map map))
         (_ (message "Map read"))
         (player-start (seq-find (lambda (thing)
                                   (= (ewd-thing-type thing) 1))
                                 (ewd-map-things map)))
         (start-x-fx (ewd-thing-x-fx player-start))
         (start-y-fx (ewd-thing-y-fx player-start))
         (render-angle-bam (eldoom--thing-angle-bam player-start)))
    (setq eldoom-globals-level-time 0
          eldoom-globals-game-tic 255)
    (let ((time (car (benchmark-run 1
                       (dotimes (_ 1000)
                         (let* ((offset-fx (eldoom-math-fixed-from-float (* 96.0
                                                                            (sin (/ eldoom-globals-level-time
                                                                                    24.0)))))
                                (x-fx (+ start-x-fx (eldoom-math-mul offset-fx
                                                                     (eldoom-math-cos render-angle-bam))))
                                (y-fx (+ start-y-fx (eldoom-math-mul offset-fx
                                                                     (eldoom-math-sin render-angle-bam)))))
                           (eldoom-render-render-level map scene eldoom-canvas x-fx y-fx nil render-angle-bam
                                                       eldoom-player)
                           (cl-incf eldoom-globals-level-time)
                           (cl-incf eldoom-globals-game-tic)))))))
      (message "Rendered 1000 frames in %.3f seconds. Average %.1f FPS" time (/ 1000.0 time)))))

(defconst eldoom-player-move-speed #x19)
(defconst eldoom-player-move-speed-fast #x32)
(defconst eldoom-player-strafe-speed #x18)
(defconst eldoom-player-strafe-speed-fast #x28)
(defconst eldoom-player-turn-bam (ash 640 16))
(defconst eldoom-player-turn-fast-bam (ash 1280 16))

(defun eldoom--render-view (map scene canvas)
  (let* ((player-mobj (emo-player-mobj eldoom-player))
         (x (emo-x-fx player-mobj))
         (y (emo-y-fx player-mobj))
         (z (emo-player-view-z eldoom-player))
         (angle (emo-angle player-mobj)))
    (eldoom-render-render-level map scene canvas x y z angle eldoom-player)))

(defvar-local eldoom--forward-down-start-time nil)
(defvar-local eldoom--forward-down-last-time 0)
(defun eldoom-move-forward ()
  (setf (eldoom-user-command-forward-move eldoom-user-cmd) eldoom-player-move-speed))
(defun eldoom-move-forward-key ()
  (interactive)
  (when (not eldoom--forward-down-start-time)
    (setq eldoom--forward-down-start-time eldoom-globals-game-tic))
  (setq eldoom--forward-down-last-time eldoom-globals-game-tic))
(defvar-local eldoom--forward-fast-down-start-time nil)
(defvar-local eldoom--forward-fast-down-last-time 0)
(defun eldoom-move-forward-fast ()
  (setf (eldoom-user-command-forward-move eldoom-user-cmd) eldoom-player-move-speed-fast))
(defun eldoom-move-forward-fast-key ()
  (interactive)
  (when (not eldoom--forward-fast-down-start-time)
    (setq eldoom--forward-fast-down-start-time eldoom-globals-game-tic))
  (setq eldoom--forward-fast-down-last-time eldoom-globals-game-tic))

(defvar-local eldoom--backward-down-start-time nil)
(defvar-local eldoom--backward-down-last-time 0)
(defun eldoom-move-backward ()
  (setf (eldoom-user-command-forward-move eldoom-user-cmd) (- eldoom-player-move-speed)))
(defun eldoom-move-backward-key ()
  (interactive)
  (when (not eldoom--backward-down-start-time)
    (setq eldoom--backward-down-start-time eldoom-globals-game-tic))
  (setq eldoom--backward-down-last-time eldoom-globals-game-tic))
(defvar-local eldoom--backward-fast-down-start-time nil)
(defvar-local eldoom--backward-fast-down-last-time 0)
(defun eldoom-move-backward-fast ()
  (setf (eldoom-user-command-forward-move eldoom-user-cmd) (- eldoom-player-move-speed-fast)))
(defun eldoom-move-backward-fast-key ()
  (interactive)
  (when (not eldoom--backward-fast-down-start-time)
    (setq eldoom--backward-fast-down-start-time eldoom-globals-game-tic))
  (setq eldoom--backward-fast-down-last-time eldoom-globals-game-tic))

(defvar-local eldoom--turn-left-down-start-time nil)
(defvar-local eldoom--turn-left-down-last-time 0)
(defun eldoom-turn-left ()
  (setf (eldoom-user-command-angle-turn eldoom-user-cmd) eldoom-player-turn-bam))
(defun eldoom-turn-left-key ()
  (interactive)
  (when (not eldoom--turn-left-down-start-time)
    (setq eldoom--turn-left-down-start-time eldoom-globals-game-tic))
  (setq eldoom--turn-left-down-last-time eldoom-globals-game-tic))

(defvar-local eldoom--turn-left-fast-down-start-time nil)
(defvar-local eldoom--turn-left-fast-down-last-time 0)
(defun eldoom-turn-left-fast ()
  (setf (eldoom-user-command-angle-turn eldoom-user-cmd) eldoom-player-turn-fast-bam))
(defun eldoom-turn-left-fast-key ()
  (interactive)
  (when (not eldoom--turn-left-fast-down-start-time)
    (setq eldoom--turn-left-fast-down-start-time eldoom-globals-game-tic))
  (setq eldoom--turn-left-fast-down-last-time eldoom-globals-game-tic))

(defvar-local eldoom--turn-right-down-start-time nil)
(defvar-local eldoom--turn-right-down-last-time 0)
(defun eldoom-turn-right ()
  (setf (eldoom-user-command-angle-turn eldoom-user-cmd) (- eldoom-player-turn-bam)))
(defun eldoom-turn-right-key ()
  (interactive)
  (when (not eldoom--turn-right-down-start-time)
    (setq eldoom--turn-right-down-start-time eldoom-globals-game-tic))
  (setq eldoom--turn-right-down-last-time eldoom-globals-game-tic))

(defvar-local eldoom--turn-right-fast-down-start-time nil)
(defvar-local eldoom--turn-right-fast-down-last-time 0)
(defun eldoom-turn-right-fast ()
  (setf (eldoom-user-command-angle-turn eldoom-user-cmd) (- eldoom-player-turn-fast-bam)))
(defun eldoom-turn-right-fast-key ()
  (interactive)
  (when (not eldoom--turn-right-fast-down-start-time)
    (setq eldoom--turn-right-fast-down-start-time eldoom-globals-game-tic))
  (setq eldoom--turn-right-fast-down-last-time eldoom-globals-game-tic))

(defvar-local eldoom--strafe-right-down-start-time nil)
(defvar-local eldoom--strafe-right-down-last-time 0)
(defun eldoom-strafe-right ()
  (setf (eldoom-user-command-side-move eldoom-user-cmd) eldoom-player-strafe-speed))
(defun eldoom-strafe-right-key ()
  (interactive)
  (when (not eldoom--strafe-right-down-start-time)
    (setq eldoom--strafe-right-down-start-time eldoom-globals-game-tic))
  (setq eldoom--strafe-right-down-last-time eldoom-globals-game-tic))

(defvar-local eldoom--strafe-right-fast-down-start-time nil)
(defvar-local eldoom--strafe-right-fast-down-last-time 0)
(defun eldoom-strafe-right-fast ()
  (setf (eldoom-user-command-side-move eldoom-user-cmd) eldoom-player-strafe-speed-fast))
(defun eldoom-strafe-right-fast-key ()
  (interactive)
  (when (not eldoom--strafe-right-fast-down-start-time)
    (setq eldoom--strafe-right-fast-down-start-time eldoom-globals-game-tic))
  (setq eldoom--strafe-right-fast-down-last-time eldoom-globals-game-tic))

(defvar-local eldoom--strafe-left-down-start-time nil)
(defvar-local eldoom--strafe-left-down-last-time 0)
(defun eldoom-strafe-left ()
  (setf (eldoom-user-command-side-move eldoom-user-cmd) (- eldoom-player-strafe-speed)))
(defun eldoom-strafe-left-key ()
  (interactive)
  (when (not eldoom--strafe-left-down-start-time)
    (setq eldoom--strafe-left-down-start-time eldoom-globals-game-tic))
  (setq eldoom--strafe-left-down-last-time eldoom-globals-game-tic))

(defvar-local eldoom--strafe-left-fast-down-start-time nil)
(defvar-local eldoom--strafe-left-fast-down-last-time 0)
(defun eldoom-strafe-left-fast ()
  (setf (eldoom-user-command-side-move eldoom-user-cmd) (- eldoom-player-strafe-speed-fast)))
(defun eldoom-strafe-left-fast-key ()
  (interactive)
  (when (not eldoom--strafe-left-fast-down-start-time)
    (setq eldoom--strafe-left-fast-down-start-time eldoom-globals-game-tic))
  (setq eldoom--strafe-left-fast-down-last-time eldoom-globals-game-tic))

(defun eldoom-use ()
  "Use a special linedef in front of the player (space / use key)."
  (interactive)
  (setf (eldoom-user-command-action-button eldoom-user-cmd) t))

(defvar-local eldoom--scene nil)

(defvar-local eldoom--max-tic-time 0)

(defconst eldoom--update-period (/ 1.0 eldoom-globals-tic-rate))

(defmacro eldoom--update-button (start-time last-time)
  `(when ,start-time
     (when (< (- eldoom-globals-game-tic ,start-time) eldoom-globals-min-move-time-tics)
       (setq ,last-time eldoom-globals-game-tic))
     (when (/= ,last-time eldoom-globals-game-tic)
       (setq ,start-time nil))))

(defun eldoom--tic ()
  (eldoom--with-game-buffer
   (unless (with-local-quit
             (let ((tic-start (float-time)))
	       (eldoom--update-button eldoom--forward-down-start-time
				      eldoom--forward-down-last-time)
	       (eldoom--update-button eldoom--forward-fast-down-start-time
				      eldoom--forward-fast-down-last-time)
	       (eldoom--update-button eldoom--backward-down-start-time
				      eldoom--backward-down-last-time)
	       (eldoom--update-button eldoom--backward-fast-down-start-time
				      eldoom--backward-fast-down-last-time)
	       (eldoom--update-button eldoom--backward-fast-down-start-time
				      eldoom--backward-fast-down-last-time)
	       (eldoom--update-button eldoom--turn-left-down-start-time
				      eldoom--turn-left-down-last-time)
	       (eldoom--update-button eldoom--turn-left-fast-down-start-time
				      eldoom--turn-left-fast-down-last-time)
	       (eldoom--update-button eldoom--turn-right-down-start-time
				      eldoom--turn-right-down-last-time)
	       (eldoom--update-button eldoom--turn-right-fast-down-start-time
				      eldoom--turn-right-fast-down-last-time)
	       (eldoom--update-button eldoom--strafe-left-down-start-time
				      eldoom--strafe-left-down-last-time)
	       (eldoom--update-button eldoom--strafe-left-fast-down-start-time
				      eldoom--strafe-left-fast-down-last-time)
	       (eldoom--update-button eldoom--strafe-right-down-start-time
				      eldoom--strafe-right-down-last-time)
	       (eldoom--update-button eldoom--strafe-right-fast-down-start-time
				      eldoom--strafe-right-fast-down-last-time)
	       (cond
		((= eldoom--forward-down-last-time eldoom-globals-game-tic)
		 (eldoom-move-forward))
		((= eldoom--forward-fast-down-last-time eldoom-globals-game-tic)
		 (eldoom-move-forward-fast))
		((= eldoom--backward-down-last-time eldoom-globals-game-tic)
		 (eldoom-move-backward))
		((= eldoom--backward-fast-down-last-time eldoom-globals-game-tic)
		 (eldoom-move-backward-fast))
		((= eldoom--turn-left-down-last-time eldoom-globals-game-tic)
		 (eldoom-turn-left))
		((= eldoom--turn-left-fast-down-last-time eldoom-globals-game-tic)
		 (eldoom-turn-left-fast))
		((= eldoom--turn-right-down-last-time eldoom-globals-game-tic)
		 (eldoom-turn-right))
		((= eldoom--turn-right-fast-down-last-time eldoom-globals-game-tic)
		 (eldoom-turn-right-fast))
		((= eldoom--strafe-left-down-last-time eldoom-globals-game-tic)
		 (eldoom-strafe-left))
		((= eldoom--strafe-left-fast-down-last-time eldoom-globals-game-tic)
		 (eldoom-strafe-left-fast))
		((= eldoom--strafe-right-down-last-time eldoom-globals-game-tic)
		 (eldoom-strafe-right))
		((= eldoom--strafe-right-fast-down-last-time eldoom-globals-game-tic)
		 (eldoom-strafe-right-fast)))
	       (when eldoom--mouse-left-down-p
                 (setf (eldoom-user-command-attack-button eldoom-user-cmd) t))

	       (eldoom-user-update-player eldoom-player)
	       (eth-run-thinkers)
	       (eldoom-special-update)	    
	       (eldoom--render-view ewd-current-map eldoom--scene eldoom-canvas)
	       (eldoom-render eldoom-canvas)
	       (redisplay t)
	       (cl-incf eldoom-globals-level-time)
	       (cl-incf eldoom-globals-game-tic)
	       (let* ((elapsed (- (float-time) tic-start))
                      (delay (max (- eldoom--update-period elapsed) 0.001))) ;; TODO: at least 1ms between timer calls
	         (cl-callf max eldoom--max-tic-time elapsed)
	         (when (zerop (% eldoom-globals-level-time 35))
	           (message "eldoom tic: %.3fs %d,%d"
		            eldoom--max-tic-time
                            (emo-x-fx (emo-player-mobj eldoom-player))
                            (emo-y-fx (emo-player-mobj eldoom-player)))
	           (setq eldoom--max-tic-time 0))
                 (unless quit-flag
	           (setq eldoom--tic-timer (run-at-time delay nil #'eldoom--tic))))
	       t)
             )
     (eldoom-quit))))

(defun eldoom ()
  (interactive)
  (eldoom-init)
  (let* ((_ (message "Reading wad directory from %s" eldoom-wad-path))
         (directory (ewd-read-directory eldoom-wad-path))
         (map-name "MAP01")
         (_ (message "Reading map %s" map-name))
         (map (ewd-load-map eldoom-wad-path directory map-name))
         (scene (eldoom-render-scene-for-map map))
         (_ (message "Map read"))
         (player-start (seq-find (lambda (thing)
                                   (= (ewd-thing-type thing) 1))
                                 (ewd-map-things map))))
    (eldoom--with-game-buffer
     (setq eldoom-globals-level-time 0
           eldoom-globals-game-tic (random 256)
           ewd-current-map map
           eldoom--scene scene
           eldoom-globals-difficulty :easy)
     (eldoom-random-init)
     (enf-init (ewd-map-sprites map))
     (eth-init)
     (eldoom-special-init map)
     (emo-init-things map)
     (eldoom--init-player-at-thing player-start)
     (eac-init-actions enf-states)
     (eldoom-user-init eldoom-player enf-states)
     (etr-init)
     (eph-init))
    ;; (eldoom-math-set-flagf (emo-player-cheats eldoom-player) eldoom-globals-CF_GODMODE)
    (garbage-collect)
    (eldoom--tic)))

;; Local Variables:
;; read-symbol-shorthands: (("ewd-" . "eldoom-wad-") ("emp-" . "eldoom-map-") ("emo-" . "eldoom-mobj-") ("enf-" . "eldoom-info-") ("eph-" . "eldoom-phys-") ("eth-" . "eldoom-think-") ("etr-". "eldoom-trace-") ("eac-" . "eldoom-actor-"))
;; End:
(provide 'eldoom)
;;; eldoom.el ends here
