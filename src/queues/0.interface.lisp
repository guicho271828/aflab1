(defpackage :eazy-a-star.queue
  (:use :cl :ea*.b)
  (:nicknames :ea*.q)
  (:export
   #:queue-methods))
(in-package :ea*.q)

(deftype init (queue)
  `(function (&optional priority) ,queue))
(deftype enqueue (queue)
  `(function (,queue node priority) (values)))
(deftype dequeue (queue)
  `(function (,queue) (values (or node null) boolean)))
(deftype delete-node (queue)
  `(function (,queue priority node) (values ,queue boolean)))
(deftype delete-id (queue)
  `(function (,queue priority id) (values ,queue boolean)))

(define-interface queue-methods (queue)
  "Interface for priority queues"
  init enqueue dequeue delete-node delete-id)
