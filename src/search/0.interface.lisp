(defpackage eazy-a-star.search
  (:use :cl :ea*.b :structure-interface)
  (:export :solution-not-found
           :solution-found
           :solution
           :search-condition
           :search-interface)
  (:nicknames :ea*.s))

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

(define-interface search-interface ()
  ((expand `(function (t t distance successor)
                      (function (node) (values))))
   (fetch  `(function (t) (function () node))))
  :export t
  :documentation "search engine interface")



