;;; hash-bag

(defpackage :eazy-a-star.bag.hash
  (:use :cl :ea*.b :structure-interface :trivia)
  (:shadowing-import-from :immutable-struct :ftype)
  (:nicknames :ea*.bag.h)
  (:export
   #:hash-bag))
(in-package :ea*.bag.h)

(defstruct hash-bag
  (hash (make-hash-table) :type hash-table))

(implement-interface
    (ea*.bag:bag-interface hash-bag node))

(defun init ()
  (make-hash-bag))

(defun-ematch emptyp (hb)
  ((hash-bag hash)
   (zerop (hash-table-count hash))))

(defun insert (hb node)
  (ematch hb
    ((hash-bag hash)
     (setf (gethash (id node) hash) node)
     hb)))

(defun delete-id (hb id)
  (ematch hb
    ((hash-bag hash)
     (remhash id hash)
     hb)))

(defun-ematch get1 (hb)
  ((hash-bag hash)
   (with-hash-table-iterator (node hash)
     (return-from get1 (nth-value 2 (node))))))

(defun map-bag (hb fn)
  (ematch hb
    ((hash-bag hash)
     (with-hash-table-iterator (node hash)
       (funcall fn (nth-value 2 (node))))
     (values))))
