;;; eldoom.el --- Doom engine implemented in emacs lisp -*- lexical-binding:t -*-

;; Copyright (C) 2022 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Invoke (eldoom) to run Doom inside emacsd

;;; Code:

(require 'eldoom-actor)
(require 'eldoom-canvas)
(require 'eldoom-config)
(require 'eldoom-globals)
(require 'eldoom-info)
(require 'eldoom-map)
(require 'eldoom-math)
(require 'eldoom-mobj)
(require 'eldoom-random)
(require 'eldoom-render)
(require 'eldoom-special)
(require 'eldoom-think)
(require 'eldoom-trace)
(require 'eldoom-user)
(require 'eldoom-wad)

(eval-when-compile (require 'cl-lib))
(require 'profiler)
(require 'seq)

;; (byte-recompile-directory "~/src/eldoom" 0 t)

(defvar-local eldoom-player nil)

(defvar-local eldoom-canvas nil)
(defvar-local eldoom-quitting nil)
(defvar-local eldoom-frame-gc-cons-threshold (* 100 1024 1024))
;;   "Temporary `gc-cons-threshold' used while rendering animation frames.")
(defvar-local eldoom--saved-gc-cons-threshold nil)
(defvar-local eldoom--saved-mouse-color nil)
(defvar-local eldoom--saved-x-pointer-shape nil)
(defvar-local eldoom--timer nil)


(defvar-local eldoom--mouse-preset-x nil)
(defvar-local eldoom--mouse-position-jitter 0)
(defvar-local eldoom--mouse-sensitivity 5000000)

(defun eldoom--mouse-move ()
  (interactive)
  (let* ((pos (mouse-pixel-position))
         (x (cadr pos))
         (y (cddr pos))
         (left (window-pixel-left))
         (top (window-pixel-top)))
    (when (and (<= 0 x (+ left (1- (window-pixel-width)))) (<= 0 y (+ (1- (window-pixel-height)) top)))
      (when eldoom--mouse-preset-x
        (let ((dx (- x eldoom--mouse-preset-x)))
          (cl-decf (eldoom-user-command-angle-turn eldoom-user-cmd) (* dx eldoom--mouse-sensitivity))))
      (setq eldoom--mouse-preset-x (+ left (/ eldoom-canvas-scaled-width 2)))
      (set-mouse-pixel-position (selected-frame)
                                eldoom--mouse-preset-x
                                (+ eldoom--mouse-position-jitter top (/ eldoom-canvas-scaled-height 2)))
      (setq eldoom--mouse-position-jitter (- 1 eldoom--mouse-position-jitter)))))

(defun eldoom--mouse-left-down ()
  (interactive)
  (message "Attack")
  (setf (eldoom-user-command-attack-button eldoom-user-cmd) t))

(defun eldoom--handle-any-key ()
  (interactive))

(defvar eldoom-mode-map nil)
(setq eldoom-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "<left>") #'eldoom-turn-left)
        (define-key map (kbd "a") #'eldoom-strafe-left)
        (define-key map (kbd "A") #'eldoom-strafe-left-fast)
        (define-key map (kbd "S-<left>") #'eldoom-turn-left-fast)
        (define-key map (kbd "<right>") #'eldoom-turn-right)
        (define-key map (kbd "d") #'eldoom-strafe-right)
        (define-key map (kbd "D") #'eldoom-strafe-right-fast)
        (define-key map (kbd "S-<right>") #'eldoom-turn-right-fast)
        (define-key map (kbd "M-<left>") #'eldoom-strafe-left)
        (define-key map (kbd "M-S-<left>") #'eldoom-strafe-left-fast)
        (define-key map (kbd "M-<right>") #'eldoom-strafe-right)
        (define-key map (kbd "M-S-<right>") #'eldoom-strafe-right-fast)
        (define-key map (kbd "<up>") #'eldoom-move-forward)
        (define-key map (kbd "M-<up>") #'eldoom-move-forward)
        (define-key map (kbd "S-<up>") #'eldoom-move-forward-fast)
        (define-key map (kbd "M-S-<up>") #'eldoom-move-forward-fast)
        (define-key map (kbd "w") #'eldoom-move-forward)
        (define-key map (kbd "W") #'eldoom-move-forward-fast)
        (define-key map (kbd "<down>") #'eldoom-move-backward)
        (define-key map (kbd "M-<down>") #'eldoom-move-backward)
        (define-key map (kbd "S-<down>") #'eldoom-move-backward-fast)
        (define-key map (kbd "M-S-<down>") #'eldoom-move-backward-fast)
        (define-key map (kbd "s") #'eldoom-move-backward)
        (define-key map (kbd "S") #'eldoom-move-backward-fast)
        (define-key map (kbd "<SPC>") #'eldoom-use)
        (define-key map (kbd "M-<SPC>") #'eldoom-use)
        (define-key map (kbd "S-<SPC>") #'eldoom-use)
        (define-key map (kbd "M-S-<SPC>") #'eldoom-use)
        (define-key map (kbd "q") #'eldoom-quit)
        (define-key map (kbd "C-g") #'eldoom-quit)
        (define-key map [mouse-movement] #'eldoom--mouse-move)
        (define-key map [down-mouse-1] #'eldoom--mouse-left-down)
        (define-key map [double-down-mouse-1] #'eldoom--mouse-left-down)
        (define-key map [triple-down-mouse-1] #'eldoom--mouse-left-down)
        (define-key map [t] #'eldoom--handle-any-key)
        map))

(define-derived-mode eldoom-mode special-mode "ELDooom"
  (setq-local overriding-local-map eldoom-mode-map
              track-mouse t
              mouse-fine-grained-tracking t
              cursor-type nil
              make-pointer-invisible t
              mouse-highlight nil)
  (buffer-disable-undo)
  (add-hook 'kill-buffer-hook #'eldoom-quit nil 'local))

(defun eldoom-init ()
  (setq debug-on-error t)
  (unless eldoom--saved-gc-cons-threshold
    (setq eldoom--saved-gc-cons-threshold gc-cons-threshold))
  (unless eldoom--saved-mouse-color
    (setq eldoom--saved-mouse-color (alist-get 'mouse-color (frame-parameters))))
  (unless eldoom--saved-x-pointer-shape
    (setq eldoom--saved-x-pointer-shape x-pointer-shape))
  (setq x-pointer-shape x-pointer-invisible)
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
  (interactive)
  (unless eldoom-quitting
    (message "Quitting eldoom")
    (setq eldoom-quitting t)
    (when (timerp eldoom--timer)
      (cancel-timer eldoom--timer))
    (when eldoom--saved-gc-cons-threshold
      (setq gc-cons-threshold eldoom--saved-gc-cons-threshold
            eldoom--saved-gc-cons-threshold nil))
    (setq x-pointer-shape eldoom--saved-x-pointer-shape)
    (set-mouse-color eldoom--saved-mouse-color)
    (when (get-buffer eldoom-globals-buffer-name)
      (kill-buffer eldoom-globals-buffer-name)
      (clear-image-cache))
    (setq debug-on-error nil)))
;;(quit-window t)))

(defvar eldoom-wad-path nil
  "Default WAD path used by ELDoom test helpers.")

(if (eq system-type 'windows-nt)
    (setq eldoom-wad-path "c:/wad/freedoom2.wad")
  ;; (setq eldoom-wad-path "c:/wad/DOOM2.WAD")
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

(defun eldoom-move-forward ()
  (interactive)
  (setf (eldoom-user-command-forward-move eldoom-user-cmd) eldoom-player-move-speed))
(defun eldoom-move-forward-fast ()
  (interactive)
  (setf (eldoom-user-command-forward-move eldoom-user-cmd) eldoom-player-move-speed-fast))

(defun eldoom-move-backward ()
  (interactive)
  (setf (eldoom-user-command-forward-move eldoom-user-cmd) (- eldoom-player-move-speed)))
(defun eldoom-move-backward-fast ()
  (interactive)
  (setf (eldoom-user-command-forward-move eldoom-user-cmd) (- eldoom-player-move-speed-fast)))

(defun eldoom-turn-left ()
  (interactive)
  (setf (eldoom-user-command-angle-turn eldoom-user-cmd) eldoom-player-turn-bam))
(defun eldoom-turn-left-fast ()
  (interactive)
  (setf (eldoom-user-command-angle-turn eldoom-user-cmd) eldoom-player-turn-fast-bam))

(defun eldoom-turn-right ()
  (interactive)
  (setf (eldoom-user-command-angle-turn eldoom-user-cmd) (- eldoom-player-turn-bam)))
(defun eldoom-turn-right-fast ()
  (interactive)
  (setf (eldoom-user-command-angle-turn eldoom-user-cmd) (- eldoom-player-turn-fast-bam)))

(defun eldoom-strafe-right ()
  (interactive)
  (setf (eldoom-user-command-side-move eldoom-user-cmd) eldoom-player-strafe-speed))
(defun eldoom-strafe-right-fast ()
  (interactive)
  (setf (eldoom-user-command-side-move eldoom-user-cmd) eldoom-player-strafe-speed-fast))

(defun eldoom-strafe-left ()
  (interactive)
  (setf (eldoom-user-command-side-move eldoom-user-cmd) (- eldoom-player-strafe-speed)))
(defun eldoom-strafe-left-fast ()
  (interactive)
  (setf (eldoom-user-command-side-move eldoom-user-cmd) (- eldoom-player-strafe-speed-fast)))

(defun eldoom-use ()
  "Use a special linedef in front of the player (space / use key)."
  (interactive)
  (setf (eldoom-user-command-action-button eldoom-user-cmd) t))

;; TODO: try finding a way make emacs do it by itself instead of us simulating
;; emacs input dispatching
(defun eldoom--dispatch-selected-buffer-input ()
  "Run pending key bindings in the selected window's buffer.
Used while the blocking game loop runs and another buffer is focused."
  (let* ((target-window (selected-window))
         (target-buffer (window-buffer target-window))
         (game-buffer (get-buffer eldoom-globals-buffer-name)))
    (unless (and game-buffer (eq target-buffer game-buffer))
      (with-current-buffer target-buffer
        (while (input-pending-p)
          (let* ((keys (read-key-sequence-vector nil nil t nil t)))
            (when (length> keys 0)
              (unwind-protect
                  (let ((cmd (key-binding keys)))
                    (when (commandp cmd)
                      (call-interactively cmd)))
                (set-buffer target-buffer)))))))))


(defun eldoom--inject-mouse-move (x y)
  "Inject a synthetic mouse-movement event at frame pixel (X, Y)."
  (when-let* ((posn (posn-at-x-y x y (selected-frame) t)))
    (push (list 'mouse-movement posn) unread-command-events)))

(defun eldoom--mouse-coordinate (event)
  (nth 2 (cadr event)))
(defun eldoom--process-input ()
  ;; Only drain unread input while the game buffer owns the selected window.
  ;; `read-key-sequence-vector' reads the global event queue and would
  ;; otherwise steal keys meant for the buffer the user switched to.
  (when (eldoom--game-buffer-selected-p)
    ;;(track-mouse
    (let ((game-buffer (current-buffer)))
      (let* ((keys (read-key-sequence-vector nil nil t nil t)))
        (when (length> keys 0)
          (unwind-protect
              (cond ((equal keys [?a]) (eldoom-strafe-left))
                    ((equal keys [?A]) (eldoom-strafe-left-fast))
                    ((equal keys [?d]) (eldoom-strafe-right))
                    ((equal keys [?D]) (eldoom-strafe-right-fast))
                    ((equal keys [?w]) (eldoom-move-forward))
                    ((equal keys [?W]) (eldoom-move-forward-fast))
                    ((equal keys [?s]) (eldoom-move-backward))
                    ((equal keys [?S]) (eldoom-move-backward-fast))
                    ((equal keys [?q]) (eldoom-turn-left))
                    ((equal keys [?S]) (eldoom-turn-left-fast))
                    ((equal keys [?e]) (eldoom-turn-right))
                    ((equal keys [?E]) (eldoom-turn-right-fast))
                    ((equal keys [?\s]) (eldoom-use))
                    ;; cursor keys have inverted move/strafe logic vs AWSD
                    ;; as I'm just more used to it...
                    ((equal keys [left]) (eldoom-turn-left-fast))
                    ((equal keys [M-left]) (eldoom-strafe-left-fast))
                    ((equal keys [right]) (eldoom-turn-right-fast))
                    ((equal keys [M-right]) (eldoom-strafe-right-fast))
                    ((equal keys [up]) (eldoom-move-forward-fast))
                    ((equal keys [M-up]) (eldoom-move-forward-fast))
                    ((equal keys [down]) (eldoom-move-backward-fast))
                    ((equal keys [M-down]) (eldoom-move-backward-fast))
                    ((equal keys [prior]) (eldoom-strafe-left-fast))
                    ((equal keys [next]) (eldoom-strafe-right-fast))

                    ((equal keys [?\C-g]) (keyboard-quit))
                    (t
                     (let ((cmd (key-binding keys)))
                       (when (commandp cmd)
                         (ignore-errors (call-interactively cmd))))))
            (set-buffer game-buffer)))))))

(defun eldoom--tic-time ()
  (car (time-convert (time-stamp) eldoom-globals-tic-rate)))

(defvar-local eldoom--scene nil)

(defun eldoom--tic ()
  (eldoom--with-game-buffer
   (unless (with-local-quit
             (eldoom-user-update-player eldoom-player)
             (eth-run-thinkers)
             (eldoom-special-update)
             (cl-incf eldoom-globals-level-time)
             (cl-incf eldoom-globals-game-tic)
             (eldoom--render-view ewd-current-map eldoom--scene eldoom-canvas)
             (eldoom-render eldoom-canvas)
             (redisplay t)
             (sit-for 0.001)
             t)
     (eldoom-quit))))

(defun eldoom-timer-based ()
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
     (etr-init))
    (eldoom-math-set-flagf (emo-player-cheats eldoom-player) eldoom-globals-CF_GODMODE)
    (garbage-collect)
    (when (timerp eldoom--timer)
      (cancel-timer eldoom--timer))
    (setq eldoom--timer
          (run-at-time nil (/ 1.0 35.0) #'eldoom--tic))))

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
                                 (ewd-map-things map)))
         (max-render-time 0)
         (last-time (eldoom--tic-time)))
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
     (let ((left (window-pixel-left))
           (top (window-pixel-top)))
       (set-mouse-pixel-position (selected-frame) (+ left (/ eldoom-canvas-scaled-width 2))
                                 (+ top (/ eldoom-canvas-scaled-height 2)))))
    ;; (eldoom-math-set-flagf (emo-player-cheats eldoom-player) eldoom-globals-CF_GODMODE)
    (garbage-collect)
    (condition-case nil
        (while (not eldoom-quitting)
          (let* ((now (eldoom--tic-time))
                 (tic-processed nil))

            (eldoom--with-game-buffer
             ;; 1. THE TIC LOOP (State Updates)
             (while (< last-time now)
               (eldoom--process-input)
               (eldoom-user-update-player eldoom-player)
               (eth-run-thinkers)
               (eldoom-special-update)
               (cl-incf eldoom-globals-level-time)
               (cl-incf eldoom-globals-game-tic)
               (cl-incf last-time)
               (setq tic-processed t))

             ;; 2. THE RENDER LOOP
             ;; Only render if the game state actually advanced
             (unless eldoom-quitting
               (when tic-processed
                 (let ((render-start (float-time)))
                   (eldoom--render-view ewd-current-map scene eldoom-canvas)
                   (setq max-render-time (max max-render-time (- (float-time) render-start)))
                   (when (zerop (% eldoom-globals-level-time 20))
                     (message "eldoom render: %.3fs %d,%d" max-render-time
                              (emo-x-fx (emo-player-mobj eldoom-player))
                              (emo-y-fx (emo-player-mobj eldoom-player)))
                     (setq max-render-time 0))
                   (eldoom-render eldoom-canvas)
                   (redisplay t))))

             (unless (eldoom--game-buffer-selected-p)
               (eldoom--dispatch-selected-buffer-input))
             ;; `sleep-for' yields without reading the input queue; `sit-for'
             ;; would consume keys meant for the focused buffer.
             (sleep-for 0.001))))
      (quit
       (eldoom-quit)))))

;; Local Variables:
;; read-symbol-shorthands: (("ewd-" . "eldoom-wad-") ("emp-" . "eldoom-map-") ("emo-" . "eldoom-mobj-") ("enf-" . "eldoom-info-") ("eth-" . "eldoom-think-") ("etr-". "eldoom-trace-") ("eac-" . "eldoom-actor-"))
;; End:
(provide 'eldoom)
;;; eldoom.el ends here
