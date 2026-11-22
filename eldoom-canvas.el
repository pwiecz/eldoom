;;; eldoom-canvas.el --- Displaying graphics -*- lexical-binding:t -*-

;; Copyright (C) 2022 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1

(eval-when-compile (require 'cl-lib))
(require 'image)

(require 'eldoom-config)
(require 'eldoom-globals)

(cl-defstruct (eldoom-canvas--canvas
               (:constructor eldoom-canvas--make-canvas)
               (:copier nil))
  data-offset
  width
  height
  bitmap
  image)

(defun eldoom-create-canvas (width height)
  (unless (image-type-available-p 'pbm)
    (error "PBM images not supported"))
  (let* ((header (concat "P6 " (number-to-string width) " " (number-to-string height) " 255\n"))
	 (data (string-to-unibyte
		(concat header (make-string (* width height 3) 0))))
	 (image (create-image data 'pbm t :width width :height height :pointer 'text :foreground "black" :background "white" :transform-smoothing nil)))
    ;; Use text as pointer type, to hide the mouse pointer, as in eldoom-init
    ;; we set x-pointer-shape to x-pointer-invisible
    (setq image-scaling-factor 1)
    (eldoom-canvas--make-canvas
     :data-offset (length header)
     :width width
     :height height
     :bitmap data
     :image image)))

(defun eldoom-canvas-data-offset (canvas)
  (eldoom-canvas--canvas-data-offset canvas))

(defun eldoom-canvas-bitmap (canvas)
  (eldoom-canvas--canvas-bitmap canvas))

(defun eldoom-draw-pixel (x y color canvas)
  (let* ((data-offset (eldoom-canvas-data-offset canvas))
	 (offset (+ data-offset (* 3 (+ x (* y eldoom-canvas-width)))))
	 (bitmap (eldoom-canvas--canvas-bitmap canvas)))
    (aset bitmap offset (aref color 0))
    (aset bitmap (1+ offset) (aref color 1))
    (aset bitmap (+ 2 offset) (aref color 2))))

(defvar-local eldoom-canvas--last-win-width -1)
(defvar-local eldoom-canvas--last-win-height -1)

(defun eldoom-render (canvas)
  (with-current-buffer eldoom-globals-buffer-name
    (setq inhibit-read-only t)
    (let* ((image (eldoom-canvas--canvas-image canvas))
           (window (get-buffer-window (current-buffer))))
      
      ;; Scale the image if the buffer is currently visible in a window
      (when window
        (let* ((win-width (window-body-width window t))  ; 't' requests width in pixels
               (win-height (window-body-height window t))) ; 't' requests height in pixels
          (when (or (/= win-width eldoom-canvas--last-win-width)
                    (/= win-height eldoom-canvas--last-win-height))
            (setq eldoom-canvas--last-win-width win-width
                  eldoom-canvas--last-win-height win-height)
            ;; Mutate the image descriptor to match the window size
            (let ((scaled-win-height (truncate (* win-width (/ (* eldoom-pixel-ratio eldoom-canvas-height) eldoom-canvas-width)))))
              (if (<= scaled-win-height win-height)
                  (setf (image-property image :width) win-width
                        (image-property image :height) scaled-win-height)
                (setf (image-property image :width) (truncate (* win-height (/ eldoom-canvas-width (* eldoom-pixel-ratio eldoom-canvas-height))))
                      (image-property image :height) win-height))))))
      
      (erase-buffer)
      (insert-image image)
      (image-flush image))
    (setq inhibit-read-only nil)))


(provide 'eldoom-canvas)
