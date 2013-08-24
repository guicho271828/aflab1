(in-package :guicho-a*)
(speed*)
;; (optimize*)
(cl-syntax:use-syntax :annot)


;; based on rb-tree

(defun make-queue ()
  (leaf))

(defun insert-queue (priority thing queue)
  (rb-insert queue priority
	     (cons thing (rb-member queue priority))))

(defun remove-queue (priority thing queue)
  (rb-insert queue (remove thing (rb-member priority queue))))