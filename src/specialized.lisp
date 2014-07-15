
(in-package :guicho-a*)
(cl-syntax:use-syntax :annot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fixed cost
(defstruct (fixed-cost-edge (:include searchable-edge)
                            (:constructor fixed-cost-edge (from to)))
  (cost 0 :type number))
(defclass fixed-cost-node (searchable-node) ())
(defmethod cost ((e fixed-cost-edge))
  @inline fixed-cost-edge-cost
  (fixed-cost-edge-cost e))
(defmethod connect ((from fixed-cost-node) (to fixed-cost-node))
  (let ((e (fixed-cost-edge from to)))
    (push e (edges from))
    e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; discrete cost
(defstruct (discrete-cost-edge (:include searchable-edge)
                               (:constructor discrete-cost-edge (from to)))
  (cost 0 :type fixnum))
(defclass discrete-cost-node (searchable-node)
  ())
(defmethod cost ((e discrete-cost-edge))
  @inline discrete-cost-edge-cost
  (discrete-cost-edge-cost e))
(defmethod connect ((from discrete-cost-node) (to discrete-cost-node))
  (let ((e (discrete-cost-edge from to)))
    (push e (edges from))
    e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; single-float cost
(defstruct (single-float-cost-edge (:include searchable-edge)
                            (:constructor single-float-cost-edge (from to)))
  (cost 0.0 :type single-float))
(defclass single-float-cost-node (searchable-node) ())
(defmethod cost ((e single-float-cost-edge))
  @inline single-float-cost-edge-cost
  (single-float-cost-edge-cost e))
(defmethod connect ((from single-float-cost-node) (to single-float-cost-node))
  (let ((e (single-float-cost-edge from to)))
    (push e (edges from))
    e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; double-float cost
(defstruct (double-float-cost-edge (:include searchable-edge)
                            (:constructor double-float-cost-edge (from to)))
  (cost 0.0d0 :type double-float))
(defclass double-float-cost-node (searchable-node) ())
(defmethod cost ((e double-float-cost-edge))
  @inline double-float-cost-edge-cost
  (double-float-cost-edge-cost e))
(defmethod connect ((from double-float-cost-node) (to double-float-cost-node))
  (let ((e (double-float-cost-edge from to)))
    (push e (edges from))
    e))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit cost

(declaim (inline unit-cost-edge-cost))
(defun unit-cost-edge-cost (instance)
  (declare (ignore instance))
  1)

(defstruct (unit-cost-edge (:include searchable-edge)
                           (:constructor unit-cost-edge (from to))))
(defclass unit-cost-node (searchable-node) ())
(defmethod cost ((e discrete-cost-edge))
  @inline discrete-cost-edge-cost
  (discrete-cost-edge-cost e))
(defmethod connect ((from unit-cost-node) (to unit-cost-node))
  (let ((e (unit-cost-edge from to)))
    (push e (edges from))
    e))


