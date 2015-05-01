
(defpackage :eazy-a-star.queue.array.hash
  (:use :cl :ea*.b :ea*.q)
  (:shadowing-import-from :immutable-struct :ftype)
  (:nicknames :ea*.q.a.h)
  (:export #:init #:enqueue #:dequeue))
(in-package :ea*.q.a.h)


(ftype init &optional priority (array hashtable))
(defun init (&optional (initial-max (expt 2 18)))
  (make-array initial-max :element-type 'hashtable))

(ftype enqueue (array hashtable) t priority (values))
(defun enqueue (queue node value)
  (unless (aref queue value)
    (setf (aref queue value) (make-hash-table)))
  (setf (gethash (id node) (aref queue value)) node)
  (values))

(ftype dequeue (array hashtable) priority (values t boolean))
(defun dequeue (queue value)
  (if (aref queue value)
      (values (pop (aref queue value)) t)
      (values nil nil)))

