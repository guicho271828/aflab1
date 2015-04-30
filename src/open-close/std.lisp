
;;; a-star
(defpackage :eazy-a-star.open-close.std
  (:use :cl)
  (:shadowing-import-from :immutable-struct :ftype :defstruct)
  (:nicknames :ea*.oc.s)
  (:export
   :oc :oc-open :oc-closed))
(in-package :ea*.oc.s)

(defstruct oc open closed)
