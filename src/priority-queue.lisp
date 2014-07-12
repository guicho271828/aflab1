(in-package :guicho-a*)
(speed*)
;; (optimize*)
(cl-syntax:use-syntax :annot)


;; based on rb-tree

@export
(defun make-queue ()
  (leaf))

@export
(defun append-queue (priority things queue)
  (rb-insert queue priority
	     (append things (rb-member priority queue))))

@export
(defun insert-queue (priority thing queue)
  (if-let ((node (rb-member-node priority queue)))
    (progn (push thing (red-black-node-content node))
	   queue)
    (rb-insert queue priority (list thing))))

@export
(defun remove-queue (priority thing queue)
  (when-let ((node (rb-member-node priority queue)))
    (removef (red-black-node-content node) thing))
  queue)

(declaim (ftype (function ((or red-black-node leaf)) fixnum) queue-length))
@export
(defun queue-length (tree)
  (declare (optimize (debug 0) (safety 0) (space 0) (speed 3)))
  (match tree
    ((rb-node _ left _ content right)
     (the fixnum
         ;;(declare (ftype (function (&rest fixnum) fixnum) +))
         (+ (the fixnum (+ (queue-length left)
                           (queue-length right)))
            (the fixnum (length (the list content))))))
    ((leaf) 0)))
