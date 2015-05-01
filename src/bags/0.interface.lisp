(defpackage :eazy-a-star.bag
  (:use :cl :ea*.b)
  (:nicknames :ea*.bag)
  (:export
   #:bag-methods))
(in-package :ea*.bag)

(define-interface bag-methods (bag)
  "Interface for priority queues"
  (emptyp    `(function (,bag) boolean))
  (insert    `(function (,bag node) ,bag))
  (delete-id `(function (,bag id) ,bag))
  (get1      `(function (,bag) node))
  (map-bag   `(function (,bag (function (node) t)) (values))))

