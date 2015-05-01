(defpackage :eazy-a-star.base
  (:use :cl :trivia)
  (:shadowing-import-from :immutable-struct :defstruct)
  (:nicknames :ea*.b)
  (:export :node :node-id :edge :edge-id
           :priority :id
           ;; 
           :implement-interface
           :define-interface))
(in-package :ea*.b)

(deftype predicate (&optional (arg t)) `(function (,arg) boolean))

(deftype equality (&optional (arg t)) `(function (,arg ,arg) boolean))

(let ((id 0))
  (declare (fixnum id))
  (defstruct node
    (id (incf id) :type fixnum)
    (parent nil :type (or null node))))

(let ((id 0))
  (declare (fixnum id))
  (defstruct edge
    (id (incf id) :type fixnum)
    (cost 0 :type fixnum)
    (to (error "no edge destination") :type edge)))


(deftype priority ()
  `(mod #.array-dimension-limit))
(deftype id () 'fixnum)

;;; interface

(defstruct interface (arguments nil :type list) (ftypes nil :type list))
(lisp-namespace:define-namespace interface interface)

(defmacro define-interface (name args &body ftypes)
  (ematch ftypes
    ((list* (and s (type string)) rest)
     `(eval-when (:compile-toplevel :load-toplevel :execute)
        (setf (symbol-interface ',name) (interface ',args ',rest))
        (defmacro ,name (,args ,@rest)
          ,(concatenate 'string 
                        s "

The macro is a dummy macro for slime integration.")
          (error "dummy macro!"))))
    (_
     `(eval-when (:compile-toplevel :load-toplevel :execute)
        (setf (symbol-interface ',name) (interface ',args ',ftypes))
        (defmacro ,name (,args ,@ftypes) "This is a dummy macro for slime integration." (error "dummy macro!"))))))

(defmacro implement-interface ((name args &body implementations))
  (ematch (symbol-interface name)
    ((interface arguments ftypes)
     (assert (= (length ftypes) (length implementations))
             nil
             "mismatch in interface/implementation")
     (assert (= (length arguments) (length args))
             nil
             "mismatch in interface arguments")
     `(declaim ,@(mapcar (lambda (ftype impl)
                           `(cl:ftype (,ftype ,@args) ,impl))
                         ftypes
                         implementations)))))
