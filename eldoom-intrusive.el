;;; eldoom-intrusive.el --- Intrusive list -*- lexical-binding:t -*-

;; Copyright (C) 2026 Piotr Wieczorek <pwiecz@gmail.com>

;; Author: Piotr Wieczorek <pwiecz@gmail.com>
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))

;;; Commentary:

;;; Code:

(defmacro define-eldoom-intrusive (prefix prev-accessor next-accessor)
  "Generate doubly-linked list operations for given struct accessors.
PREFIX is used to name the generated functions (e.g., `PREFIX-insert-after`)."
  (let ((insert-after (intern (format "%s-insert-after" prefix)))
        (insert-before (intern (format "%s-insert-before" prefix)))
        (remove (intern (format "%s-remove" prefix)))
        (next (gensym "next-"))
        (prev (gensym "prev-")))
    `(progn
       (defun ,insert-after (node new-node)
         "Insert NEW-NODE immediately after NODE."
         (let ((,next (,next-accessor node)))
           (setf (,next-accessor new-node) ,next)
           (setf (,prev-accessor new-node) node)
           (setf (,next-accessor node) new-node)
           (when ,next
             (setf (,prev-accessor ,next) new-node)))
         new-node)

       (defun ,insert-before (node new-node)
         "Insert NEW-NODE immediately before NODE."
         (let ((,prev (,prev-accessor node)))
           (setf (,prev-accessor new-node) ,prev)
           (setf (,next-accessor new-node) node)
           (setf (,prev-accessor node) new-node)
           (when ,prev
             (setf (,next-accessor ,prev) new-node)))
         new-node)

       (defun ,remove (node)
         "Remove NODE from the list."
         (let ((,prev (,prev-accessor node))
               (,next (,next-accessor node)))
           (when ,prev (setf (,next-accessor ,prev) ,next))
           (when ,next (setf (,prev-accessor ,next) ,prev))
           (setf (,prev-accessor node) nil)
           (setf (,next-accessor node) nil))
         node))))

(defmacro define-eldoom-intrusive-freelist (prefix next-accessor make-fun)
  (let ((freelist (intern (format "%s-freelist" prefix)))
        (new (intern (format "%s-new-from-freelist" prefix)))
        (remove (intern (format "%s-move-to-freelist" prefix)))
        (obj (gensym "obj-")))
    `(progn
       (defvar-local ,freelist nil)
       (defun ,new ()
         "Pop an object from freelist if it's non-empty otherwise create one"
         (if ,freelist
             (let ((,obj ,freelist))
               (setf ,freelist (,next-accessor ,obj)
                     (,next-accessor ,obj) nil)
               ,obj)
           (,make-fun)))

       (defun ,remove (o)
         (setf (,next-accessor o) ,freelist
               ,freelist o)))))

(provide 'eldoom-intrusive)

;;; eldoom-intrusive.el ends here
