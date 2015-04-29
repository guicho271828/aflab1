(in-package :eazy-a*)
(deftype predicate (&optional (arg t)) `(function (,arg) boolean))
(deftype equality (&optional (arg t)) `(function (,arg ,arg) boolean))

(defstruct node parent)
