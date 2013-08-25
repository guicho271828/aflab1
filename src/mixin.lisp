
(in-package :guicho-a*)
(cl-syntax:use-syntax :annot)

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
 of the instance.
"
(defclass searchable-node () 
  ((edges :accessor edges :initarg :edges)
   (cost :accessor cost :initarg :cost :type number
	 :initform 0) ;  MOST-POSITIVE-DOUBLE-FLOAT
   (parent :accessor parent
	   :initarg :parent
	   :initform nil)
   (complementary-edge-class
    :allocation :class
    :reader complementary-edge-class
    :initarg :complementary-edge-class
    :initform 'searchable-edge)))

@export
(defgeneric generate-nodes (searchable-node))

(defmethod slot-unbound (class (node searchable-node)
			 (slot (eql 'edges)))
  (with-slots (edges) node
    (setf edges nil)
    (mapcar
     (lambda (new)
       (connect node new))
     (generate-nodes node))))

@export '(node edge)

(defpattern node (edges parent cost)
  `(class searchable-node
	  (edges ,edges) (parent ,parent)
	  (cost ,cost)))

@export @export-accessors @doc "an edge used in lrta*/rta*
searching. any subclass of `searchable-edge' should implement a method
`cost'.  Also, accessor EDGE-TO and EDGE-FROM should return a
`searchable-node' instance."
(defclass searchable-edge ()
  ((to :accessor edge-to :initarg :to)
   (from :accessor edge-from :initarg :from)
   (complementary-node-class
    :allocation :class
    :reader complementary-node-class
    :initarg :complementary-node-class
    :initform 'searchable-node)))

(defmethod print-object ((e searchable-edge) s)
  (print-unreadable-object (e s :type t)
    (with-slots (to from) e
      (format s "~w ~:@_ → ~:@_ ~w" from to))))

(defpattern edge (from to)
  `(class searchable-edge (to ,to) (from ,from)))


@export
@doc "gives the cost between the two nodes. "
(defgeneric heuristic-cost-between
	(searchable-node-from searchable-node-to))

@export
@doc "gives the real cost of an edge. it has `+' method combination."
(defgeneric cost (searchable-edge))

@export
(defgeneric connect (searchable-node-from searchable-node-to))

(defmethod connect ((from searchable-node) (to searchable-node))
  (let ((e (make-instance (complementary-edge-class from)
			  :from from :to to)))
    (push e (edges from))
    e))

@export
(defclass searchable-bidirectional-node (searchable-node)
  ())

(defmethod connect ((from searchable-bidirectional-node)
		    (to searchable-bidirectional-node))
  (let ((e (make-instance (complementary-edge-class from)
			  :from from :to to)))
    (push e (edges from))
    (push e (edges to))
    e))


@export
(defgeneric generic-eq (thing1 thing2))

@export
(defgeneric constraint-ordering-op (node)
  (:method (node)
    0))
