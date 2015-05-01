
(defpackage :eazy-a-star.queue.array.hash
  (:use :cl :ea*.b :ea*.q)
  (:shadowing-import-from :immutable-struct :ftype)
  (:nicknames :ea*.q.a.h)
  (:export #:init #:enqueue #:dequeue))
(in-package :ea*.q.a.h)
