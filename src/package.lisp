#|
  This file is a part of guicho-a-star project.
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
	:optima)
  (:export :leaf :red-black-node
           :rb-node
           :red-black-node-color
           :red-black-node-left
           :red-black-node-label
           :red-black-node-content
           :red-black-node-right
           :red :black
           :rb-member
           :rb-member-node
           :rb-minimum-node
           :rb-minimum
           :rb-maximum-node
           :rb-maximum
           :rb-insert
           :rb-remove-minimum-node
           :rb-remove
           :rb-node-next-node
           :rb-node-previous-node
           :rb-member-node-after
           :rb-member-node-before
           :rb-member-after
           :rb-member-before
           :rb-remove-after
           :rb-remove-before
           ))

(defpackage guicho-a-star
  (:use :cl
	:iterate
	:alexandria
	:anaphora
	:guicho-utilities
        :bordeaux-threads
	:guicho-red-black-tree
	:annot
	:annot.class
	:annot.doc
	:optima
	:optima.extra)
  (:export :edge :node
           :searchable-edge
           :searchable-edge-to
           :searchable-edge-from
           :searchable-edge-p
           :edge-to
           :edge-from
           :unit-cost-node
           :unit-cost-edge
           :unit-cost-edge-cost)
  (:nicknames :guicho-a*))
(in-package :guicho-a-star)

;; blah blah blah.

(cl-syntax:use-syntax :annot)
