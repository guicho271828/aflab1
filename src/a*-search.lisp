(in-package :aflab1)
(optimize*)
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
    (match tree
      ((tree known-cost _ node)
       (+ known-cost
	  (heuristic-cost-between node end))))))

@export
(defun a*-search (start end)
  (%a*-rec end (list (tree 0 nil start)) nil 
	   (heuristic-value-to end)))

;; fully tail-recursive

(defun %a*-rec (end open closed h)
  (match (findmin open :key h)
    ((and tree (tree _ _ node))
     (if (eq node end)
	 tree
	 (%iter-edge end open (cons tree closed) h tree (edges node))))
    (_ (error "there is no possible path!"))))

(defun %iter-edge (end open closed h now edges)
  (if (null edges)
      (%a*-rec end open closed h)
      (match now
	((tree now-cost _ _)
	 (match edges
	   ((list* (and e 
			(edge _ (and neighbor
				     (tree (place neighbor-cost)
					   (place parent) _))))
		   rest)
	    (let ((cost (+ now-cost
			   (funcall h neighbor)
			   (cost e))))
	      (cond
		((find neighbor open)
		 (when (< cost neighbor-cost)
		   (setf neighbor-cost cost)
		   (setf parent now))
		 (%iter-edge end open closed h now rest))
		
		((find neighbor closed)
		 (if (<= neighbor-cost cost)
		     (%iter-edge end open closed h now rest)
		     (progn
		       (setf neighbor-cost cost)
		       (setf parent now)
		       (%iter-edge end
				   (cons neighbor open)
				   (remove neighbor closed)
				   h now rest))))
		(t (%iter-edge end
			       (cons neighbor open)
			       closed h now rest))))))))))