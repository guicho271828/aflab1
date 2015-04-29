
(in-package :eazy-a*)
(cl-syntax:use-syntax :annot)

;; generic functions

@export
@doc "Generate node instances.
Users do not call this function.
It is implicitly  called by a method `generate-edges' which is called by
 `slot-unbound' on slot `edges' of a node."
(defgeneric generate-nodes (searchable-node))

@export
@doc "Generate edge instances.
Users do not call this function.
It is implicitly  called by a method `slot-unbound' on slot `edges' of a node."
(defgeneric generate-edges (searchable-node))

@export
@doc "gives the heuristic cost (not real cost) between the two nodes."
(defgeneric heuristic-cost-between
    (searchable-node-from searchable-node-to))

@export
@doc "gives the real cost of an edge."
(defgeneric cost (searchable-edge))

@export
(defgeneric connect (from to))

@export
(defgeneric generic-eq (thing1 thing2))

@export
(defgeneric constraint-ordering-op (node)
  (:method (node)
    0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mixins

@export
@export-accessors
@doc "a node used in lrta*/rta* searching. any subclass of
`searchable-node' should implement a method HEURISTIC-COST-BETWEEN.
also, it must provide an accessor EDGES, which returns its edges stored
in the slot, or the edges newly created in each call.

By default, a call to SLOT-UNBOUND with its third argument EDGES will 
call GENERATE-NODES, connect the nodes to itself and
 finally return the edges. It would be necessary to warn that
if GENERATE-NODES allows many duplicated instances in the meaning of
GENERIC-EQ, then the heap exhaust may occur. Users should manually
check for the duplicates in a way that fits to the structure
 of the instance. "
(defclass searchable-node () 
  ((edges :accessor edges :initarg :edges)
   (cost :accessor cost :initarg :cost :type number
         :initform 0)
   (parent :accessor parent
           :initarg :parent
           :initform nil)))

(defmethod reinitialize-instance ((instance searchable-node)
                                  &rest initargs
                                  &key &allow-other-keys)
  (apply #'shared-initialize instance nil :cost 0 :parent nil initargs))

(defmethod slot-unbound (class (node searchable-node)
                         (slot (eql 'edges)))
  (with-slots (edges) node
    (setf edges nil)
    (generate-edges node)))

(defmethod generate-edges ((node searchable-node))
  (map 'list
       (lambda (new)
         (connect node new))
       (generate-nodes node)))

(defstruct (searchable-edge (:constructor searchable-edge (from to)))
  "Edges for a*/lrta*/rta* etc.
Any subclass of `searchable-edge' should implement a method
`cost'.  Also, accessor EDGE-TO and EDGE-FROM should return a
`searchable-node' instance."
  from
  to)

(setf (fdefinition 'edge-to) (function searchable-edge-to)
      (fdefinition 'edge-from) (function searchable-edge-from))

(defmethod print-object ((e searchable-edge) s)
  (print-unreadable-object (e s :type t)
    (with-slots (to from) e
      (format s "~w ~:@_ → ~:@_ ~w" from to))))

;; connect

(defmethod connect ((from searchable-node) (to searchable-node))
  (let ((e (searchable-edge from to)))
    (push e (edges from))
    e))
