
(in-package :eazy-a-star)

(deftype predicate (&optional (arg t)) `(function (,arg) boolean))

(deftype equality (&optional (arg t)) `(function (,arg ,arg) boolean))




