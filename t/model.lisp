
(in-package :eazy-a-star-test)

(defclass 2d-node (fixed-cost-node)
  ((x :accessor x :initarg :x :type number)
   (y :accessor y :initarg :y :type number)))

(defmethod generic-eq ((n1 2d-node) (n2 2d-node))
  (and (= (x n1) (x n2))
       (= (y n1) (y n2))))

(defun 2d (x y &optional edges parent)
  (make-instance '2d-node
     :x x :y y :edges edges
     :parent parent))

(eval-when (:compile-toplevel
            :load-toplevel
            :execute)
  (defpattern 2d (x y)
    `(class 2d-node (x ,x) (y ,y))))

(defmethod heuristic-cost-between ((from 2d-node) (to 2d-node))
  (ematch to
    ((2d x1 y1)
     (ematch from
       ((2d x2 y2)
        (sqrt (+ (^2 (- x2 x1)) (^2 (- y2 y1)))))))))

(defmethod connect ((from 2d-node) (to 2d-node))
  (let ((e (call-next-method)))
    (setf (cost e)
          (ematch to
            ((2d x1 y1)
             (ematch from
               ((2d x2 y2)
                (sqrt (+ (^2 (- x2 x1)) (^2 (- y2 y1)))))))))
    e))

