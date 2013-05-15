
(in-package :aflab1-test)
(def-suite :8puzzle :in :aflab1)
(in-suite :8puzzle)

(test :generate-nodes
  (is (= 2
	 (length
	  (generate-nodes
	   (8puzzle +goal-state+ 0)))))
  (is (= 4
  	 (length
  	  (edges
  	   (8puzzle (vector 4 1 2 3 0 5 6 7 8) 4))))))

(test :make-instance
  (is (eq (find-class 'dijkstra-8puzzle)
	  (class-of (8puzzle +goal-state+ 0 'dijkstra-8puzzle))))
  (is (eq (find-class 'diff-8puzzle)
	  (class-of 
	   (random-elt
	    (generate-nodes
	     (8puzzle +goal-state+ 0
		      'diff-8puzzle)))))))


(defun move-random (8p)
  (random-elt (generate-nodes 8p)))

(test :dijkstra
  (let ((goal (8puzzle +goal-state+ 0 'dijkstra-8puzzle)))
    (is (zerop (heuristic-cost-between
		goal
		(move-random goal))))))
(test :diff
  (let ((goal (8puzzle +goal-state+ 0 'diff-8puzzle)))
    (is (= 2 (heuristic-cost-between
	      goal
	      (move-random goal))))))
(test :manhattan
  (let ((goal (8puzzle +goal-state+ 0 'manhattan-8puzzle)))
    (is (= 2 (heuristic-cost-between
	      goal
	      (move-random goal))))))

(defparameter *random-move* 9)

(defun make-problem (start)
  (let ((end start))
    (iter (with prev = nil)
	  (generate i below *random-move*)
	  (for candidate = (move-random start))
	  (when (and prev (generic-eq prev candidate))
	    (next-iteration))
	  (setf prev start)
	  (setf start candidate)
	  (next i))
    (list start end)))

(test :make-problem
  (finishes
    (make-problem
     (8puzzle (vector 0 1 2 3 4 5 6 7 8) 0
	      'dijkstra-8puzzle))))

(defparameter *repeat* 100)

(defun solve-puzzle (class)
  (let ((args (make-problem
	       (8puzzle (vector 0 1 2 3 4 5 6 7 8) 0
			class))))
    (apply #'a*-search args)))

(test solve-manhattan-8puzzle
  (print :manhattan)
  (time
   (iter (repeat *repeat*)
	 (finishes
	   (solve-puzzle 'manhattan-8puzzle)))))
(test solve-diff-8puzzle
  (print :diff)
  (time
   (iter (repeat *repeat*)
	 (finishes
	   (solve-puzzle 'diff-8puzzle)))))

;; bad performance
;; (test solve-dijkstra-8puzzle
;;   (setf *random-move* 5)
;;   (time
;;    (iter (repeat 1)
;; 	 (finishes
;; 	   (solve-puzzle 'dijkstra-8puzzle)))))