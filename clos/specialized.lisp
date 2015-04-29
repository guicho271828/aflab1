
(in-package :eazy-a*)
(cl-syntax:use-syntax :annot)

(defmacro define-cost-edge-node (prefix type default)
  (let* ((edge-class (symbolicate prefix '-cost-edge))
         (edge-cost (symbolicate edge-class '-cost))
         (node-class (symbolicate prefix '-cost-node)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (export '(,edge-class ,edge-cost ,node-class))
       (defstruct (,edge-class (:include searchable-edge)
                               (:constructor ,edge-class (from to)))
         (cost ,default :type ,type))
       (defclass ,node-class (searchable-node) ())
       (defmethod cost ((e ,edge-class))
         (declare (inline ,edge-cost))
         (,edge-cost e))
       (defmethod (setf cost) (new (e ,edge-class))
         (declare (inline (setf ,edge-cost)))
         (setf (,edge-cost e) new))
       (defmethod connect ((from ,node-class) (to ,node-class))
         (let ((e (,edge-class from to)))
           (push e (edges from))
           e)))))

(define-cost-edge-node fixed number 0)
(define-cost-edge-node discrete fixnum 0)
(define-cost-edge-node single-float single-float 0.0)
(define-cost-edge-node double-float double-float 0.0d0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit cost

(declaim (inline unit-cost-edge-cost))
(defun unit-cost-edge-cost (instance)
  (declare (ignore instance))
  1)

(defstruct (unit-cost-edge (:include searchable-edge)
                           (:constructor unit-cost-edge (from to))))
(defclass unit-cost-node (searchable-node) ())
(defmethod cost ((e unit-cost-edge))
  @inline unit-cost-edge-cost
  (unit-cost-edge-cost e))
(defmethod connect ((from unit-cost-node) (to unit-cost-node))
  (let ((e (unit-cost-edge from to)))
    (push e (edges from))
    e))


