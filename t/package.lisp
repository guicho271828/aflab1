
(in-package :cl-user)
(defpackage guicho-a-star-test
  (:use :cl
	:guicho-a*
	:guicho-red-black-tree
	:alexandria
	:iterate
	:anaphora
	:vecto
	:guicho-utilities
	:local-time
	:optima
	:fiveam)
  (:shadow :rotate :fail))

(in-package :guicho-a-star-test)
(def-suite :guicho-a-star)
