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

(define-interface queue-interface (queue bag)
  ((init        `(function ((eql ',bag)) (,queue ,bag)))
   (enqueue     `(function ((,queue ,bag) priority node) (,queue ,bag)))
   (dequeue     `(function ((,queue ,bag)) (values (or node null) boolean)))
   (delete-node `(function ((,queue ,bag) priority node) (values (,queue ,bag) boolean)))
   (delete-id   `(function ((,queue ,bag) priority id) (values (,queue ,bag) boolean))))
  :documentation "Interface for priority queues")
