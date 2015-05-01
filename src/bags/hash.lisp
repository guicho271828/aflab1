;;; hash-bag

(defpackage :eazy-a-star.bag.hash
  (:use :cl :ea*.b :ea*.bag)
  (:shadowing-import-from :immutable-struct :ftype)
  (:nicknames :ea*.bag.h)
  (:export))
(in-package :ea*.bag.h)

(implement-interface
    (ea*.bag:bag-methods (hash-table)))

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

;; (ftype init &optional priority (array hashtable))
;; (defun init (&optional (initial-max (expt 2 18)))
;;   (make-array initial-max :element-type 'hashtable))
;; 
;; (ftype enqueue (array hashtable) t priority (values))
;; (defun enqueue (queue node value)
;;   (unless (aref queue value)
;;     (setf (aref queue value) (make-hash-table)))
;;   (setf (gethash (id node) (aref queue value)) node)
;;   (values))
;; 
;; (ftype dequeue (array hashtable) priority (values t boolean))
;; (defun dequeue (queue value)
;;   (if (aref queue value)
;;       (values (pop (aref queue value)) t)
;;       (values nil nil)))

