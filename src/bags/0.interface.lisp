(defpackage :eazy-a-star.bag
  (:use :cl :ea*.b)
  (:nicknames :ea*.bag)
  (:export
   #:bag-methods))
(in-package :ea*.bag)

(deftype emptyp (bag)
  `(function (,bag) boolean))

(deftype insert (bag)
  `(function (,bag node) ,bag))

(deftype delete-id (bag)
  `(function (,bag id) ,bag))

(deftype get1 (bag)
  `(function (,bag) node))

(deftype map-bag (bag)
  `(function (,bag (function (node) t)) (values)))

(define-interface bag-methods (bag)
  "Interface for priority queues"
  emptyp insert delete-id get1 map-bag)

