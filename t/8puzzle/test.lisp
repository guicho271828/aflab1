
(in-package :guicho-a-star-test)
(def-suite :8puzzle :in :guicho-a-star)
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

(defun solve-puzzle (start-end &optional (verbose t))
  (let (cost lasts)
    (handler-bind
        ((path-not-found
          (lambda (c)
            (declare (ignorable c))
            (format t "Completely searched the state space!")))
         (solution-found
          (lambda (c)
            (let ((last (solution c)))
              (format
               t "~%Solution : ~w Cost : ~w"
               (iter
                 (for parent first (parent last) then (parent parent))
                 (for node first last then (parent node))
                 (while parent)
                 (collect (aref (state node) (position 0 (state parent)))
                   at beginning))
               (cost last))
              (push last lasts)
              (cond
                ((null cost) (setf cost (cost last)) (continue))
                ((= cost (cost last))
                 (continue))
                ((< cost (cost last))
                 ;; (let ((lasts (reverse lasts)))
                 ;;   (break+ lasts (eq (first lasts) (second lasts))))
                 (format t "~% Found all optimal paths. Search finished!"))
                (t
                 (format t "~% What happened? ~%~w"  (reverse lasts))))))))
      (destructuring-bind (start end) start-end
        (funcall #'a*-search start end :verbose verbose)))))

(defun problem-generator (class)
  (lambda () 
    (make-problem
     (8puzzle (vector 0 1 2 3 4 5 6 7 8) 0
              class))))

(test solve-manhattan-8puzzle
  (print :manhattan)
  (time 
   (for-all ((start-end (problem-generator 'manhattan-8puzzle)))
     (format t "~&verbose")
     (finishes (solve-puzzle start-end t))
     (format t "~&silent")
     (finishes (solve-puzzle start-end nil)))))

(test solve-diff-8puzzle
  (print :diff)
  (time
   (for-all ((start-end (problem-generator 'diff-8puzzle)))
     (format t "~&verbose")
     (finishes (solve-puzzle start-end t))
     (format t "~&silent")
     (finishes (solve-puzzle start-end nil)))))


;; bad performance
;; (test solve-dijkstra-8puzzle
;;   (setf *random-move* 5)
;;   (time
;;    (iter (repeat 1)
;; 	 (finishes
;; 	   (solve-puzzle 'dijkstra-8puzzle)))))
