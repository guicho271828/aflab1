(defpackage :eazy-a-star.bag
  (:use :cl :ea*.b :structure-interface)
  (:nicknames :ea*.bag))
(in-package :ea*.bag)

(define-interface bag-interface (bag content)
  ((init      `(function ()     ,bag) :external t)
   (emptyp    `(function (,bag) boolean))
   (insert    `(function (,bag ,content) ,bag))
   (delete-id `(function (,bag id) ,bag))
   (get1      `(function (,bag) ,content))
   (map-bag   `(function (,bag (function (,content) t)) (values))))
  :export t
  :documentation "Interface for bags")

