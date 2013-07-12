
(in-package :guicho-a*)
(cl-syntax:use-syntax :annot)

@export
@export-accessors
@doc "a node used in lrta*/rta* searching. any subclass of
`searchable-node' should implement a method HEURISTIC-COST-BETWEEN.
also, it must provide an accessor EDGES."
(defclass searchable-node () 
  ((edges :accessor edges :initarg :edges)
   (cost :accessor cost :initarg :cost :type number
	 :initform 0)
   (parent :accessor parent
	   :initarg :parent
	   :initform nil)
   (complementary-edge-class
    :allocation :class
    :reader complementary-edge-class
    :initarg :complementary-edge-class
    :initform (find-class 'searchable-edge))))

@export
(defun node (edges parent cost)
  (make-instance 'searchable-node
		 :edges edges :parent parent :cost cost))
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
    :initform (find-class 'searchable-node))))

@export
(defun edge (from to)
  (%edge 'searchable-edge from to))
(defun %edge (class from to)
  (make-instance class :from from :to to))
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