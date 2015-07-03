(defpackage :eazy-a-star.queue
  (:use :cl :ea*.b :structure-interface)
  (:nicknames :ea*.q)
  (:export
   #:queue-interface
   #:init
   #:enqueue
   #:dequeue
   #:delete-node
   #:delete-id))
(in-package :ea*.q)

(define-interface queue-interface (queue)
  ((init        `(function (&rest t) ,queue))
   (enqueue     `(function (,queue priority node) ,queue))
   (dequeue     `(function (,queue) (values (or node null) boolean)))
   (delete-node `(function (,queue priority node) (values ,queue boolean)))
   (delete-id   `(function (,queue priority id) (values ,queue boolean))))
  :documentation "Interface for priority queues")
