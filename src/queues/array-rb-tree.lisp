
(defpackage :eazy-a-star.queue.array.rb
  (:use :cl :trivialib.red-black-tree
        :ea*.b)
  (:shadowing-import-from :immutable-struct :ftype)
  (:nicknames :ea*.q.a.rb)
  (:export #:init #:enqueue #:dequeue))
(in-package :ea*.q.a.rb)

(defstruct queue (array (error "no array") :type (array rb-tree)))

(ftype init &optional priority queue)
(defun init (&optional (initial-max (expt 2 18)))
  (make-queue :array (make-array initial-max :element-type 'rb-tree :initial-element (leaf))))

(ftype enqueue (array rb-tree) t priority (values))
(defun enqueue (queue node value)
  (setf (aref (queue-array queue) value)
        (rb-insert (aref (queue-array queue) value) (index node) node))
  (values))

;; (ftype dequeue (array rb-tree) priority (values t boolean))
;; (defun dequeue (queue value)
;;   (ematch (aref queue value)
;;     ((leaf) (values nil nil))
;;     (it (values (rb-remove-minimum-node it) t))))

