
(in-package :cl-user)
(defpackage aflab1-test
  (:use :cl
        :aflab1
	:alexandria
	:iterate
	:anaphora
	:vecto
	:guicho-utilities
	:local-time
	:optima
	:fiveam)
  (:shadow :rotate :fail))

(in-package :aflab1-test)
(def-suite :aflab1)
