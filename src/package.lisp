#|
  This file is a part of aflab1 project.
  Copyright (c) 2013 guicho ()
|#

(in-package :cl-user)
(defpackage aflab1
  (:use :cl
	:iterate
	:alexandria
	:anaphora
	:guicho-utilities
	:annot.class
	:annot.doc
	:optima
	:optima.extra))
(in-package :aflab1)

;; blah blah blah.

(cl-syntax:use-syntax :annot)
