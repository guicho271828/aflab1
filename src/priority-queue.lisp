(in-package :guicho-a*)
;; (speed*)
(optimize*)
(cl-syntax:use-syntax :annot)


;; based on rb-tree

(defun make-queue ()
  (leaf))

(defun append-queue (priority things queue)
  (rb-insert queue priority
	     (append things (rb-member priority queue))))

(defun insert-queue (priority thing queue)
  (rb-insert queue priority
	     (cons thing (rb-member priority queue))))

(defun remove-queue (priority thing queue)
  (rb-insert queue priority
	     (remove thing (rb-member priority queue))))