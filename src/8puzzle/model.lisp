
(in-package :aflab1)
(cl-syntax:use-syntax :annot)

@export
(defvar +goal-state+ (vector 0 1 2 3 4 5 6 7 8))

@export
@export-accessors
(defclass 8puzzle-node (searchable-node)
  ((complementary-edge-class :initform '8puzzle-edge)
   (state :accessor state :initarg :state
	  :type (simple-array fixnum 9))
   (pos :accessor pos :type :fixnum :initarg :position)))

(defmethod slot-unbound :around (class (8p 8puzzle-node)
				       (slot (eql 'edges)))
  (handler-bind ((unbound-slot
		  (lambda (c)
		    (let ((edges
			   (mapcar (lambda (n)
				     (connect 8p n))
				   (generate-nodes 8p))))
		      (store-value edges c)
		      (use-value edges c)))))
    (call-next-method)))

(defmethod generic-eq ((n1 8puzzle-node) (n2 8puzzle-node))
  (equalp (state n1) (state n2)))

@export
(defun verify (8p)
  (ematch 8p
    ((8puzzle state pos)
     (assert (= (aref state pos) 0))
     t)))

@export
(defun 8puzzle (state pos &optional (class '8puzzle-node))
  (make-instance class :state state :position pos))

(defpattern 8puzzle (state pos &optional (class '8puzzle-node))
  `(class ,class (state ,state) (pos ,pos)))

(defmethod print-object ((8p 8puzzle-node) s)
  (print-unreadable-object (8p s :type t)
    (match 8p
      ((and (8puzzle (and state (vector a b c d e f g h i)) _)
	    (node _ _ cost))
       (format s "~_~<~;~a ~a ~a~%~0:t~a ~a ~a~%~0:t~a ~a ~a~;~:> :cost ~a"
	       (coerce state 'list) cost)))))

@export
(defclass 8puzzle-edge (searchable-edge)
  ((complementary-node-class :initform '8puzzle-node)))

(defmethod cost ((e 8puzzle-edge))
  1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;variants


@export
(defclass dijkstra-8puzzle (8puzzle-node) ())
(defmethod heuristic-cost-between
    ((from dijkstra-8puzzle)
     (to dijkstra-8puzzle))
  0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; difference

@export
(defclass diff-8puzzle (8puzzle-node) ())
(defmethod heuristic-cost-between
    ((from diff-8puzzle)
     (to diff-8puzzle))
  (iter (for x1 in-vector (state from))
	(for x2 in-vector (state to))
	(counting (not (= x1 x2)))))

@export
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