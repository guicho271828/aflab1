(in-package :aflab1)
(speed*)
;; (optimize*)
(cl-syntax:use-syntax :annot)

@export
(defun findmin (list &key (key #'identity))
  (iter (for elem in list)
	(finding
	 elem
	 minimizing
	 (funcall key elem))))

@export '(a*-tree tree a*-tree-p a*-tree-cost
	  a*-tree-parent a*-tree-content)

@export
(defun heuristic-value-to (end)
  (lambda (node)
    (+ (cost node)
       (heuristic-cost-between node end))))

@export
(defun a*-search (start end)
  (declare (optimize (debug 3)))
  (%a*-rec end (list start) nil 
	   (heuristic-value-to end)))

;; fully tail-recursive

(defun %a*-rec (end open closed h)
  (if-let ((node (findmin open :key h)))
    (if (generic-eq node end)
	node
	(%iter-edge end
		    (remove node open)
		    (cons node closed)
		    h node (edges node)))
    (error "there is no possible path!")))

(defun %iter-edge (end open closed h now edges)
  (if (null edges)
      (%a*-rec end open closed h)
      (ematch edges
	((list* (and e (or (edge (eq now) neighbor)
			   (edge neighbor (eq now)))) rest)
	 (let ((cost (+ (cost now) (cost e)
			(heuristic-cost-between neighbor end))))
	   (cond
	     ((find neighbor open)
	      (when (< cost (cost neighbor))
		(setf (cost neighbor) cost)
		(setf (parent neighbor) now))
	      (%iter-edge end open closed h now rest))
	     
	     ((find neighbor closed)
	      (if (<= (cost neighbor) cost)
		  (%iter-edge end open closed h now rest)
		  (progn
		    (setf (cost neighbor) cost)
		    (setf (parent neighbor) now)
		    (%iter-edge end
				(cons neighbor open)
				(remove neighbor closed)
				h now rest))))

	     (t (setf (cost neighbor) cost)
		(setf (parent neighbor) now)
		(%iter-edge
		 end
		 (cons neighbor open)
		 closed h now rest))))))))