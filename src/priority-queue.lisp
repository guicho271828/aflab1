(in-package :eazy-a*)
(speed*)
;; (optimize*)
(cl-syntax:use-syntax :annot)


;; based on rb-tree

@export
(defun make-queue ()
  (leaf))

(declaim (ftype (function (real list rb-tree) red-black-node) append-queue))
@export
(defun append-queue (priority things queue)
  (if things
      (match (rb-member-node priority queue)
        ((rb-node _ _ _ content _)
         (rb-insert queue priority (append things content)))
        (_ ; leaf
         (rb-insert queue priority things)))
      queue))

@export
(defun insert-queue (priority thing queue)
  (match (rb-member-node priority queue)
    ((rb-node _ _ _ content _)
     (rb-insert queue priority (cons thing content)))
    (nil
     (rb-insert queue priority (list thing)))))

@export
(defun pop-queue (priority queue)
  (match (rb-member-node priority queue)
    ((rb-node _ _ _ (list popped) _)
     (values popped (rb-remove queue priority)))
    ((rb-node _ _ _ (list* popped rest) _)
     (values popped (rb-insert queue priority rest)))
    (nil (values nil queue))))

@export
(defun pop-queue-minimum (queue)
  (match (rb-minimum-node queue)
    ((leaf) (values nil queue))
    ((rb-node _ _ priority (list popped) _)
     (values popped (rb-remove queue priority)))
    ((rb-node _ _ priority (list* popped rest) _)
     (values popped (rb-insert queue priority rest)))))

@export
(defun pop-queue-maximum (queue)
  (match (rb-maximum-node queue)
    ((leaf) (values nil queue))
    ((rb-node _ _ priority (list popped) _)
     (values popped (rb-remove queue priority)))
    ((rb-node _ _ priority (list* popped rest) _)
     (values popped (rb-insert queue priority rest)))))

@export
(defun remove-queue (priority thing queue)
  (match (rb-member-node priority queue)
    ((rb-node _ _ _ content _)
     (rb-insert queue priority (remove thing content)))
    (nil queue)))

(declaim (ftype (function (rb-tree) (integer 0)) queue-length))
@export
(defun queue-length (tree)
  (match tree
    ((rb-node _ left _ content right)
     (declare (type rb-tree left right))
     (declare (type list content))
     (+ (queue-length left)
        (queue-length right)
        (length content)))
    ((leaf) 0)))
