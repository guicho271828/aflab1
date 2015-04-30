
(defpackage :eazy-a-star.edge
  (:use :cl)
  (:shadowing-import-from :immutable-struct :ftype :defstruct)
  (:nicknames :ea*.e)
  (:export
   :edge
   :edge-to
   :edge-cost))
(in-package :ea*.e)

(defstruct edge cost to)


