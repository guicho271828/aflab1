(defpackage :eazy-a-star.base
  (:use :cl :trivia :structure-interface)
  (:shadowing-import-from :immutable-struct :ftype)
  (:nicknames :ea*.b)
  (:export :predicate
           :equality
           :priority
           ;;
           :id :node :make-node
           :edge
           :make-edge
           :*default-parent-node*))
(in-package :ea*.b)

(deftype predicate (&optional (arg t)) `(function (,arg) boolean))
(deftype equality (&optional (arg t)) `(function (,arg ,arg) boolean))
(deftype priority () `(mod #.array-dimension-limit))

;;; id

(defvar *id-count* 0)
(deftype id () 'fixnum)
(declaim (id *id-count*))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (immutable-struct:defstruct id-mixin
    (id (incf *id-count*) :type id)))

(declaim (inline id id-mixin-id))
(ftype id id-mixin id)
(defun id (id-mixin)
  (declare (optimize (debug 1) (speed 3) (space 0) (compilation-speed 0) (safety 0)))
  (id-mixin-id id-mixin))

;;; node and edge

;; (eval-when (:compile-toplevel :load-toplevel :execute)
;;   )
(defvar *default-parent-node*)
  (defstruct (node (:include id-mixin))
    (parent *default-parent-node* :type node))
(setf *default-parent-node* (allocate-instance (find-class 'node)))

(immutable-struct:defstruct (edge (:include id-mixin))
  (cost 0 :type fixnum)
  (to (error "no edge destination") :type edge))


