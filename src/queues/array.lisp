(defpackage :eazy-a*.queue.array
  (:use :cl :trivia)
  (:shadowing-import-from :immutable-struct :ftype))
(in-package :eazy-a*.queue.array)

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

