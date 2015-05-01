(defpackage :eazy-a-star.queue
  (:use :cl :ea*.b)
  (:nicknames :ea*.q)
  (:export
   #:queue-methods))
(in-package :ea*.q)

(define-interface queue-methods (queue)
  ((enqueue     `(function (,queue priority node) (values)))
   (dequeue     `(function (,queue) (values (or node null) boolean)))
   (delete-node `(function (,queue priority node) (values ,queue boolean)))
   (delete-id   `(function (,queue priority id) (values ,queue boolean))))
  :export t
  :documentation "Interface for priority queues")
