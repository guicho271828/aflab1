(in-package :aflab1)
(speed*)
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

(defstruct (a*-tree (:constructor tree (cost parent content)))
  cost parent content)

(defpattern tree (cost parent content)
  `(a*-tree- (cost ,cost) (parent ,parent) (content ,content)))

@export
(defun heuristic-value-to (end)
  (lambda (tree)
    (ematch tree
      ((tree known-cost _ node)
       (+ known-cost
	  (heuristic-cost-between node end))))))

@export
(defun a*-search (start end)
  (%a*-rec end (list (tree 0 nil start)) nil 
	   (heuristic-value-to end)))

;; fully tail-recursive

(defun %a*-rec (end open closed h)
  (ematch (findmin open :key h)
    ((and tree (tree _ _ node))
     (if (eq node end)
	 tree
	 (%iter-edge end
		     (remove tree open)
		     (cons tree closed)
		     h tree (edges node))))
    (_ (error "there is no possible path!"))))

(defun %iter-edge (end open closed h now edges)
  (if (null edges)
      (%a*-rec end open closed h)
      (ematch now
	((tree now-cost _ _)
	 (ematch edges
	   ((list* (and e (edge _ neighbor)) rest)
	    (let ((cost (+ now-cost (cost e)
			   (heuristic-cost-between neighbor end))))
	      (acond
		((find neighbor open)
		 (ematch it
		   ((tree (place neighbor-cost)
			  (place parent)
			  _)
		    (when (< cost neighbor-cost)
		      (setf neighbor-cost cost)
		      (setf parent now))
		    (%iter-edge end open closed h now rest))))
		
		((find neighbor closed)
		 (ematch it
		   ((tree (place neighbor-cost)
			  (place parent)
			  _)
		    (if (<= neighbor-cost cost)
			(%iter-edge end open closed h now rest)
			(progn
			  (setf neighbor-cost cost)
			  (setf parent now)
			  (%iter-edge end
				      (cons it open)
				      (remove it closed)
				      h now rest))))))
		(t (%iter-edge
		    end
		    (cons (tree cost now neighbor) open)
		    closed h now rest))))))))))