(defpackage :eazy-a-star.bag
  (:use :cl :ea*.b :structure-interface)
  (:nicknames :ea*.bag)
  (:export
   #:init
   #:emptyp
   #:insert
   #:delete-id
   #:get1
   #:map-bag
   #:bag-interface))
(in-package :ea*.bag)

(define-interface bag-interface (bag content)
  ((init      `(function ((eql ',content)) (,bag ,content)))
   (emptyp    `(function ((,bag ,content)) boolean))
   (insert    `(function ((,bag ,content) ,content) (,bag ,content)))
   (delete-id `(function ((,bag ,content) id) (,bag ,content)))
   (get1      `(function ((,bag ,content)) ,content))
   (map-bag   `(function ((,bag ,content) (function (,content) t)) (values))))
  :documentation "Interface for bags")

