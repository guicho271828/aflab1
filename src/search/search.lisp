(defpackage eazy-a-star.search
  (:use :cl :ea*.b)
  (:export :solution-not-found
           :solution-found
           :solution
           :search-condition)
  (:nicknames :ea*.s))

;;; generic forward search
(in-package :ea*.s)

;;; conditions

(define-condition solution-not-found (error)
  ()
  (:report
   (lambda (c s)
     (declare (ignore c))
     (declare (type stream s))
     (format s "Solution not found!"))))

(define-condition solution-found ()
  ((solution :initarg :solution :accessor solution :type node)))

(define-condition search-condition () ())
