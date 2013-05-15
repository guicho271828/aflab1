
(in-package :aflab1-test)
(def-suite :8puzzle :in :aflab1)
(in-suite :8puzzle)

(test generate-nodes
  (is (= (length
	  (generate-nodes
	   (8puzzle (vector 0 1 2 3 4 5 6 7 8) 0)))
	 2))
  (is (= (length
	  (edges
	   (8puzzle (vector 4 1 2 3 0 5 6 7 8) 0)))
	 4)))

(test make-instance
  (is (eq (class-of (8puzzle (vector 0 1 2 3 4 5 6 7 8) 0
			     'dijkstra-8puzzle))
	  (find-class 'dijkstra-8puzzle)))
  (is (eq (class-of 
	   (random-elt
	    (generate-nodes
	     (8puzzle (vector 0 1 2 3 4 5 6 7 8) 0
		      'diff-8puzzle))))
	  (find-class 'diff-8puzzle))))

(defun move-random (8p)
  (random-elt (generate-nodes 8p)))

(defun make-problem (start)
  (let ((end start))
    (iter (repeat 100)
	  (setf start (move-random start)))
    (list start end)))

(defun solve-puzzle (class)
  (let ((args (make-problem
		(8puzzle (vector 0 1 2 3 4 5 6 7 8) 0
			 class))))
    (pass)
    (finishes
     (time
      (apply a*-search args)))))

(test solve-dijkstra-8puzzle
      (solve-puzzle 'dijkstra-8puzzle))
(test solve-diff-8puzzle
      (solve-puzzle 'diff-8puzzle))
(test solve-manhattan-8puzzle
      (solve-puzzle 'manhattan-8puzzle))