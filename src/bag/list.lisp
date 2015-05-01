(defpackage :eazy-a-star.bag.list
  (:use :cl :ea*.b :ea*.bag)
  (:shadowing-import-from :immutable-struct :ftype)
  (:nicknames :ea*.bag.l)
  (:export))
(in-package :ea*.bag.l)

(implement-interface
    (ea*.bag:bag-methods (list)))

(defun emptyp (list)
  (null list))

(defun insert (list node)
  (push node list)
  list)

(defun delete-id (list id)
  (delete id list :count 1 :key #'node-id))

(defun get1 (list)
  (car list))

(defun map-bag (list fn)
  (mapcar fn list)
  (values))
