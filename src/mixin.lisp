
(in-package :aflab1)
(cl-syntax:use-syntax :annot)

@export
@export-accessors
@doc "a node used in lrta*/rta* searching. any subclass of
`searchable-node' should implement a method HEURISTIC-COST-BETWEEN.
also, it must provide an accessor EDGES."
(defclass searchable-node () ())
(defmethod initializing-instance :after
	((n searchable-node) &key edges)
  (when edges
	(setf (edges n) edges)))
(defgeneric edges (searchable-node))
(defgeneric (setf edges) (edges searchable-node))

@export @export-accessors @doc "an edge used in lrta*/rta*
searching. any subclass of `searchable-edge' should implement a method
`cost'.  Also, accessor EDGE-TO and EDGE-FROM should return a
`searchable-node' instance."
(defclass searchable-edge () ())
(defmethod initializing-instance :after
	((e searchable-edge) &key to from)
  (when to
	(setf (edge-to e) to))
  (when from
	(setf (edge-from e) from)))

@export
(defgeneric edge-to (searchable-edge))
@export
(defgeneric (setf edge-to) (edges searchable-edge))
@export
(defgeneric edge-from (searchable-edge))
@export
(defgeneric (setf edge-from) (edges searchable-edge))

@export
@doc "gives the cost between the two nodes. "
(defgeneric heuristic-cost-between
	(searchable-node-from searchable-node-to))
@export
@doc "gives the real cost of an edge. it has `+' method combination."
(defgeneric cost (searchable-edge)
  (:method-combination +))

@export
(defgeneric connect (searchable-node-from searchable-node-to))
