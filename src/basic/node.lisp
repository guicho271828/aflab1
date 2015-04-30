
(defpackage :eazy-a-star.edge
  (:use :cl)
  (:shadowing-import-from :immutable-struct :ftype :defstruct)
  (:nicknames :ea*.e)
  (:export))
(in-package :ea*.e)

(defstruct node (parent nil :type (or null node)))
