
(in-package :eazy-a-star-test)
(cl-syntax:use-syntax :annot)

(defvar +goal-state+ (vector 0 1 2 3 4 5 6 7 8))

(defclass 8puzzle-node (unit-cost-node)
  ((state :accessor state :initarg :state
	  :type (simple-array fixnum 9))
   (pos :accessor pos :type :fixnum :initarg :position)))

(defpattern 8puzzle (state pos &optional (class '8puzzle-node))
  `(class ,class (state ,state) (pos ,pos)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;variants

(defclass dijkstra-8puzzle (8puzzle-node) ())
(defmethod heuristic-cost-between
    ((from dijkstra-8puzzle)
     (to dijkstra-8puzzle))
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; difference
(defclass diff-8puzzle (8puzzle-node) ())
(defmethod heuristic-cost-between
    ((from diff-8puzzle)
     (to diff-8puzzle))
  (iter (for x1 in-vector (state from))
	(for x2 in-vector (state to))
	(counting (not (= x1 x2)))))

(defclass manhattan-8puzzle (8puzzle-node) ())
(defmethod heuristic-cost-between
    ((from manhattan-8puzzle)
     (to manhattan-8puzzle))
  (iter (with st = (state to))
	(for x in-vector (state from) with-index i)
	(summing
	 (let ((diff (abs (- (position x st) i))))
	   (multiple-value-bind (quotient remainder)
	       (floor diff 3)
	     (+ quotient remainder))))))
