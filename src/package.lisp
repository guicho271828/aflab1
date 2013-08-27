#|
  This file is a part of aflab1 project.
  Copyright (c) 2013 guicho ()
|#

(in-package :cl-user)

(defpackage guicho-red-black-tree
  (:use :cl
	:alexandria
	:guicho-utilities
	:annot
	:annot.class
	:annot.doc
	:optima))

(defpackage guicho-a*
  (:use :cl
	:iterate
	:alexandria
	:anaphora
	:guicho-utilities
	:guicho-red-black-tree
	:annot
	:annot.class
	:annot.doc
	:optima
	:optima.extra))

(defpackage aflab1
  (:use :cl
	:guicho-a*
	:iterate
	:alexandria
	:anaphora
	:guicho-utilities
	:annot
	:annot.class
	:annot.doc
	:optima
	:optima.extra))
(in-package :aflab1)

;; blah blah blah.

(cl-syntax:use-syntax :annot)
