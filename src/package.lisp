#|
  This file is a part of eazy-a-star project.
  Copyright (c) 2013 guicho ()
|#

(in-package :cl-user)

(defpackage eazy-a-star
  (:use :cl
	:iterate
	:alexandria
	:guicho-utilities
	:guicho-red-black-tree
	:annot
	:annot.class
	:annot.doc
	:optima
	:optima.extra)
  (:export :path-not-found
           :solution-found
           :solution
           ;;
           :edge :node
           :searchable-edge
           :searchable-edge-to
           :searchable-edge-from
           :searchable-edge-p
           :edge-to
           :edge-from
           :unit-cost-node
           :unit-cost-edge
           :unit-cost-edge-cost)
  (:nicknames :eazy-a*))
(in-package :eazy-a-star)

;; blah blah blah.

(cl-syntax:use-syntax :annot)
