;;; eldoom-think.el --- Per-tic thinkers -*- lexical-binding:t -*-

;; Copyright (C) 2026 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "24.3"))

;;; Commentary:

;; Thinker registry

;;; Code:

(eval-when-compile (require 'cl-lib))

(require 'eldoom-info)
(require 'eldoom-intrusive)
(require 'eldoom-map)
(require 'eldoom-math)
(require 'eldoom-mobj)
(require 'eldoom-phys)
(require 'eldoom-wad)

(cl-defstruct (eth-thinker (:constructor eth--make-thinker) (:copier nil))
  object
  action
  prev
  next)

(define-eldoom-intrusive eth-thinker eth-thinker-prev eth-thinker-next)

(defun eth--make-sentinel ()
  "Return a circular-list sentinel for world thinkers."
  (let ((sentinel (eth--make-thinker)))
    (setf (eth-thinker-next sentinel) sentinel
          (eth-thinker-prev sentinel) sentinel)
    sentinel))

(defvar-local eth--thinkers nil)

(define-eldoom-intrusive-freelist eldoom-think--thinker
                                  eldoom-think-thinker-next
                                  eldoom-think--make-thinker)

(defun eth--new-thinker (object action)
  (let ((thinker (eth--thinker-new-from-freelist)))
    (setf (eth-thinker-object thinker) object
          (eth-thinker-action thinker) action)
    thinker))

(defun eth-register (object action)
  "Creates a thinker object with given OBJECT and ACTION and puts it
at the back of the THINKERS list. Returns the thinker object."
  (let ((thinker (eth--new-thinker object action)))
    (eth-thinker-insert-before eth--thinkers thinker)
    thinker))

(defun eth--unregister (thinker)
  (eth-thinker-remove thinker)
  (eth--thinker-move-to-freelist thinker))

(defun eth-run-thinkers ()
  "Run one tic of every registered thinker.
Thinkers with nil action will be removed from the list."
  (let ((thinker (eth-thinker-next eth--thinkers)))
    (while (not (eq thinker eth--thinkers))
      (let ((action (eth-thinker-action thinker))
            (next-thinker (eth-thinker-next thinker)))
        (if action
            (funcall action (eth-thinker-object thinker))
          (eth--unregister thinker))
        (setq thinker next-thinker)))))

(defun eth-for-each-thinker (callback obj)
  (let ((thinker (eth-thinker-next eth--thinkers)))
    (while (not (eq thinker eth--thinkers))
      (let ((next-thinker (eth-thinker-next thinker)))
        (when (eth-thinker-action thinker)
          (funcall callback thinker obj))
        (setq thinker next-thinker)))))

(defun eth--mobj-thinker (mobj)
  "P_MobjThinker in p_mobj.c"
  (catch :return
    (when (or (/= 0 (emo-momx mobj))
              (/= 0 (emo-momy mobj))
              (eldoom-math-has-flag (emo-flags mobj) enf-MF_SKULLFLY))
      (eph-xy-movement mobj)
      (when (not (emo-state mobj))
        (throw :return t)))
    (when (or (/= (emo-z-fx mobj) (emo-floor-z mobj))
              (/= 0 (emo-momz mobj)))
      (eph-z-movement mobj)
      (when (not (emo-state mobj))
        (throw :return t)))
    (if (/= (emo-tics mobj) -1)
        (progn
          (cl-decf (emo-tics mobj))
          (when (zerop (emo-tics mobj))
            (emo-set-state mobj (enf-state-next (emo-state mobj)))))
      ;; TODO: check for nightmare respawn
      nil)))

(defun eth-register-mobj-thinker (mobj)
  (let ((thinker (eth-register mobj #'eth--mobj-thinker)))
    (setf (emo-thinker mobj) thinker)))

(defun eth-unregister-mobj-thinker (mobj)
  "Mark thinker to be removed on the next tick"
  (when-let* ((thinker (emo-thinker mobj)))
    (setf (eth-thinker-action thinker) nil)))

(defun eth-init ()
  (setq eth--thinkers (eth--make-sentinel))
  (emo-add-remove-function #'eth-unregister-mobj-thinker)
  (emo-add-spawn-function #'eth-register-mobj-thinker))


;; Local Variables:
;; read-symbol-shorthands: (("eth-" . "eldoom-think-") ("emp-" . "eldoom-map-") ("emo-" . "eldoom-mobj-") ("eph-" . "eldoom-phys-") ("enf-" . "eldoom-info-") ("ewd-" . "eldoom-wad-"))
;; End:
(provide 'eldoom-think)
;;; eldoom-think.el ends here
