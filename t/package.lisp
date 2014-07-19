
(in-package :cl-user)
(defpackage eazy-a-star-test
  (:use :cl
	:eazy-a*
	:guicho-red-black-tree
	:alexandria
	:iterate
	:vecto
	:guicho-utilities
	:local-time
	:optima
	:fiveam)
  (:shadow :rotate :fail))

(in-package :eazy-a-star-test)
(def-suite :eazy-a-star)
