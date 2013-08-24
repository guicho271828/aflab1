
(in-package :aflab1)
(cl-syntax:use-syntax :annot)

@export
@export-accessors
(defclass 2d-node (searchable-bidirectional-node)
  ((complementary-edge-class :initform '2d-edge)
   (x :accessor x :initarg :x :type number)
   (y :accessor y :initarg :y :type number)))

(defmethod generic-eq ((n1 2d-node) (n2 2d-node))
  (and (= (x n1) (x n2))
       (= (y n1) (y n2))))

@export
(defun 2d (x y &optional edges parent)
  (make-instance '2d-node
		 :x x :y y :edges edges
		 :parent parent))

(defpattern 2d (x y)
  `(class 2d-node (x ,x) (y ,y)))

@export
(defclass 2d-edge (searchable-edge)
  ((complementary-node-class :initform '2d-node)))

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

