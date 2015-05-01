;;; hash-bag

(defpackage :eazy-a-star.bag.hash
  (:use :cl :ea*.b :ea*.bag)
  (:shadowing-import-from :immutable-struct :ftype)
  (:nicknames :ea*.bag.h)
  (:export))
(in-package :ea*.bag.h)

(implement-interface
    (ea*.bag:bag-methods hash-table))

(defun emptyp (hash)
  (zerop (hash-table-count hash)))

(defun insert (hash node)
  (setf (gethash (id node) hash) node)
  hash)

(defun delete-id (hash id)
  (remhash id hash)
  hash)

(defun get1 (hash)
  (with-hash-table-iterator (node hash)
    (return-from get1 (nth-value 2 (node)))))

(defun map-bag (hash fn)
  (with-hash-table-iterator (node hash)
    (funcall fn (nth-value 2 (node))))
  (values))
