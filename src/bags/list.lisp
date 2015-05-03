(defpackage :eazy-a-star.bag.list
  (:use :cl :ea*.b :structure-interface :trivia)
  (:shadowing-import-from :immutable-struct :ftype)
  (:nicknames :ea*.bag.l)
  (:export
   #:list-bag))
(in-package :ea*.bag.l)

(defstruct list-bag
  (list nil :type list))

(implement-interface
    (ea*.bag:bag-interface list-bag node))

(defun init ()
  (make-list-bag))

(defun-ematch emptyp (lb)
  ((list-bag list)
   (null list)))

(defun insert (lb node)
  (ematch lb
    ((list-bag (list (place list)))
     (push node list)
     lb)))

(defun delete-id (lb id)
  (ematch lb
    ((list-bag (list (place list)))
     (setf list (delete id list :count 1 :key #'id))
     lb)))

(defun-ematch get1 (lb)
  ((list-bag list)
   (pop list)))

(defun map-bag (lb fn)
  (ematch lb
    ((list-bag list)
     (mapcar fn list)
     (values))))
