;;; list-bag
(defpackage :eazy-a-star.queue.array.list
  (:use :cl)
  (:shadowing-import-from :immutable-struct :ftype)
  (:nicknames :ea*.q.a.l)
  (:export
   #:init
   #:enqueue
   #:dequeue))
(in-package :ea*.q.a.l)

;; Assumes the histogram of the priority is very dense and the priority is
;; always an integer.
;; 
;; best for unit cost problems.

;; when not unit-cost, use gcd to try converting the costs?

;; (expt 2 26) array: up to 67108864, 400M
;; (expt 2 20) array: up to 1048576, 10M
;; (expt 2 18) array: up to 262144, 2.5M
;; array-dimension-limit
;; array-rank-limit
;; array-total-size-limit

(ftype init &optional (mod #.array-dimension-limit) (array list))
(defun init (&optional (initial-max (expt 2 18)))
  (make-array initial-max :element-type 'list :initial-element nil))

(ftype enqueue (array list) t (mod #.array-dimension-limit) (values))
(defun enqueue (queue thing value)
  (push (aref queue value) thing)
  (values))

(ftype dequeue (array list) (mod #.array-dimension-limit) (values t boolean))
(defun dequeue (queue value)
  (if (aref queue value)
      (values (pop (aref queue value)) t)
      (values nil nil)))


;;; 
;; b-tree based bag within each f-var

(defpackage :eazy-a-star.queue.array.btree
  (:use :cl :eazy-a-star.bag.btree)
  (:shadowing-import-from :immutable-struct :ftype)
  (:nicknames :ea*.q.a.b)
  (:export
   #:init
   #:enqueue
   #:dequeue))
(in-package :ea*.q.a.b)

(ftype init &optional (mod #.array-dimension-limit) (array btree))
(defun init (&optional (initial-max (expt 2 18)))
  (make-array initial-max :element-type 'btree))

(ftype enqueue (array btree) t (mod #.array-dimension-limit) (values))
(defun enqueue (queue thing value)
  (push (aref queue value) thing)
  (values))

(ftype dequeue (array btree) (mod #.array-dimension-limit) (values t boolean))
(defun dequeue (queue value)
  (if (aref queue value)
      (values (pop (aref queue value)) t)
      (values nil nil)))
