(in-package :guicho-a*)
(speed*)
;; (optimize*)
(cl-syntax:use-syntax :annot)


;; based on rb-tree

(defun make-queue ()
  (leaf))

(defun append-queue (priority things queue)
  (rb-insert queue priority
	     (append things (rb-member priority queue))))

(defun insert-queue (priority thing queue)
  (if-let ((node (rb-member-node priority queue)))
    (progn (push thing (red-black-node-content node))
	   queue)
    (rb-insert queue priority (list thing))))

(defun remove-queue (priority thing queue)
  (when-let ((node (rb-member-node priority queue)))
    (removef (red-black-node-content node) thing))
  queue)
