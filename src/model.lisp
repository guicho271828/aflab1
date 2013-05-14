
(in-package :aflab1)
(cl-syntax:use-syntax :annot)

@export
@export-accessors
(defclass 2d-node (searchable-node)
  ((edges :accessor edges :initarg :edges)
   (x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))

@export
(defclass 2d-edge (searchable-edge)
  ((to :accessor edge-to :initarg :to)
   (from :accessor edge-from :initarg :from)))

@export
(defun 2d (x y &optional edges)
  (make-instance '2d-node :x x :y y :edges edges))
(defpattern 2d (x y &optional (edges '_))
  `(class 2d-node (x ,x) (y ,y) (edges ,edges)))

@export
(defun edge (from to)
  (make-instance '2d-edge :from from :to to))
(defpattern edge (from to)
  `(class searchable-edge (to ,to) (from ,from)))

(defmethod connect ((from 2d-node) (to 2d-node))
  (let ((e (make-instance '2d-edge :to to :from from)))
    (push e (edges from))
    (push e (edges to))
    e))

(defmethod heuristic-cost-between ((from 2d-node) (to 2d-node))
  (match to
    ((2d x1 y1 _)
     (match from
       ((2d x2 y2 _)
	(sqrt (+ (^2 (- x2 x1)) (^2 (- y2 y1)))))))))

(defmethod cost + ((e 2d-edge))
	   (match e
	     ((edge to from)
	      (match to
		((2d x1 y1)
		 (match from
		   ((2d x2 y2)
		    (sqrt (+ (^2 (- x2 x1)) (^2 (- y2 y1)))))))))))

