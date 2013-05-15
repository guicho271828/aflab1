
(in-package :aflab1)
(cl-syntax:use-syntax :annot)

@export
@export-accessors
(defclass 2d-node (searchable-node)
  ((x :accessor x :initarg :x :type number)
   (y :accessor y :initarg :y :type number)))

@export
(defun 2d (x y &optional edges parent (cost MOST-POSITIVE-FIXNUM))
  (make-instance '2d-node
		 :x x :y y :edges edges
		 :parent parent :cost cost))
(defpattern 2d (x y)
  `(class 2d-node (x ,x) (y ,y)))

@export
(defclass 2d-edge (searchable-edge) ())

(defun 2d-edge (from to)
  (%edge '2d-edge from to))

(defmethod connect ((from 2d-node) (to 2d-node))
  (let ((e (2d-edge from to)))
    (push e (edges from))
    (push e (edges to))
    e))

(defmethod heuristic-cost-between ((from 2d-node) (to 2d-node))
  (ematch to
    ((2d x1 y1)
     (ematch from
       ((2d x2 y2)
	(sqrt (+ (^2 (- x2 x1)) (^2 (- y2 y1)))))))))

(defmethod cost ((e 2d-edge))
  (ematch e
    ((edge to from)
     (ematch to
       ((2d x1 y1)
	(ematch from
	  ((2d x2 y2)
	   (sqrt (+ (^2 (- x2 x1)) (^2 (- y2 y1)))))))))))

