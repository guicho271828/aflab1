
(in-package :aflab1)
(cl-syntax:use-syntax :annot)

@export
@export-accessors
(defclass 8puzzle-node (searchable-node)
  ((state :accessor state :initarg :state
	  :type (simple-array fixnum 9))
   (pos :accessor pos :type :fixnum :initarg :position)))

(defmethod print-object ((8p 8puzzle-node) s)
  (print-unreadable-object (8p s :type t)
    (match 8p
      ((8puzzle state _)
       (princ state s)))))

(defmethod slot-unbound (class
			 (8p 8puzzle-node)
			 (slot-name (eql 'edges)))
  (let ((nodes (generate-nodes 8p)))
    (setf (edges 8p)
	  (mapcar (lambda (n)
	      (connect 8p n))
	    nodes))))	

@export
(defun 8puzzle (state pos &optional (class '8puzzle-node))
  (make-instance
   class
   :state state :position pos))
(defpattern 8puzzle (state pos &optional (class '8puzzle-node))
  `(class ,class (state ,state) (pos ,pos)))

@export
(defclass 8puzzle-edge (searchable-edge) ())
(defun 8puzzle-edge (from to)
  (%edge '8puzzle-edge from to))

(defmethod connect ((from 8puzzle-node) (to 8puzzle-node))
  (let ((e (8puzzle-edge to from)))
    (push e (edges from))
    (push e (edges to))
    e))

(defmethod cost ((e 8puzzle-edge))
  1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;variants


@export
(defclass dijkstra-8puzzle (8puzzle-node) ())
(defmethod heuristic-cost-between ((from dijkstra-8puzzle)
				   (to dijkstra-8puzzle))
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; difference

@export
(defclass diff-8puzzle (8puzzle-node) ())
(defmethod heuristic-cost-between ((from dijkstra-8puzzle)
				   (to dijkstra-8puzzle))
  (iter (for x1 in-vector (state from))
	(for x2 in-vector (state to))
	(counting (eq x1 x2))))


@export
(defclass manhattan-8puzzle (8puzzle-node) ())
(defmethod heuristic-cost-between ((from dijkstra-8puzzle)
				   (to dijkstra-8puzzle))
  (iter (with st = (state to))
	(for x in-vector (state from) with-index i)
	(summing
	 (let ((diff (abs (- (position x st) i))))
	   (multiple-value-bind (quotient remainder)
	       (floor diff 3)
	     (+ quotient remainder))))))