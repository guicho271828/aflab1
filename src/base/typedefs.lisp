(defpackage :eazy-a-star.base
  (:use :cl :trivia :structure-interface)
  (:shadowing-import-from :immutable-struct :defstruct :ftype)
  (:nicknames :ea*.b)
  (:export :predicate
           :equality
           :priority
           :distance
           :cost
           :successor
           ;;
           :id :node :edge
           :goalp-interface
           ;;
           :implement-interface
           :define-interface))
(in-package :ea*.b)

(deftype predicate (&optional (arg t)) `(function (,arg) boolean))
(deftype equality (&optional (arg t)) `(function (,arg ,arg) boolean))
(deftype priority () `(mod #.array-dimension-limit))
(deftype distance (&optional (node 'node)) `(function (,node) fixnum))
(deftype cost () `(function (edge) fixnum))
(deftype successor (&optional (node 'node)) `(function (,node) (vector edge)))

;;; id

(defvar *id-count* 0)
(deftype id () 'fixnum)
(declaim (id *id-count*))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct id-mixin
    (id (incf *id-count*) :type id)))

(declaim (inline id id-mixin-id))
(ftype id id-mixin id)
(defun id (id-mixin)
  (declare (optimize (debug 1) (speed 3) (space 0) (compilation-speed 0) (safety 0)))
  (id-mixin-id id-mixin))

;;; node and edge

(defstruct (node (:include id-mixin))
  (parent nil :type (or null node)))

(defstruct (edge (:include id-mixin))
  (cost 0 :type fixnum)
  (to (error "no edge destination") :type edge))

;;; goal interface

(define-interface goalp-interface (node)
  ((goalp `(function (&rest *) (function (,node) boolean))))
  :export t
  :documentation "Interface for Goal Conditions")
