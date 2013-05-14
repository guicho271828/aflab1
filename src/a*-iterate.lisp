

;; iteration ver, does not compile because of macrolet and iterate. poor.
(defun a*-search (start end)
    (iter (with h = (heuristic-value-to end))
	  (with open = (list start))
	  (for (known-cost node) = (findmin open :key h))
	  (when (eq node end)
	    (collecting node)
	    (terminate))

	  (collecting node into closed)
	  (iter (for e in (edges node))
		(for neighbor = (edge-to e))
		(for cost = (+ known-cost
			       (funcall h neighbor)
			       (cost e)))
		(acond
		 ((find neighbor open)
		  (match it
		    ((tree (place known-cost)
			   (place parent) _)
		     (when (< cost known-cost)
		       (setf known-cost cost)
		       (setf place now)))))
		 ((find neighbor closed)
		  (remove it closed)
		  (push it open)
		  (match tree
		    ((tree (place known-cost)
			   (place parent) _)
		     (when (< cost known-cost)
		       (setf known-cost cost)
		       (setf place now)))))
		 (t (push (tree cost now neighbor) open))))))
