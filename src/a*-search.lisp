(in-package :guicho-a*)
;; (speed*)
(optimize*)
(cl-syntax:use-syntax :annot)

;; @export
;; (defun findmin (list &key (key #'identity))
;;   (iter (for elem in list)
;; 	(finding
;; 	 elem
;; 	 minimizing
;; 	 (funcall key elem))))

@export '(a*-tree tree a*-tree-p a*-tree-cost
	  a*-tree-parent a*-tree-content)

@export
(defun heuristic-value-to (end)
  (let ((cost 0))
    (lambda (node)
      (let ((new-cost
	     (+ (cost node)
		(heuristic-cost-between node end))))
	(when (< cost new-cost)
	  (format t "~%opened f^*(n) = ~a" new-cost)
	  (setf cost new-cost))
	new-cost))))

@export
(defun a*-search (start end)
  (declare (optimize (debug 3)))
  (let ((h (heuristic-value-to end)))
    (%a*-rec end
	     (rb-insert (leaf) (funcall h start) (list start))
	     (leaf)
	     h)))

;; fully tail-recursive

@export
(define-condition path-not-found (error)
  ()
  (:report
   (lambda (c s)
     @ignore c
     (format s "a*-search: there is no possible path!"))))

(defun %a*-rec (end open closed h)
  (match (rb-minimum-node open)
    ((rb-node _ _ _ nil _)
     (%a*-rec end (rb-remove-minimum-node open) closed h))
    ((rb-node _ _ cost (list* now rest) _)
     (if (generic-eq now end)
	 now
	 (%iter-edge end
		     (rb-insert open cost rest)
		     (rb-insert closed cost now)
		     h now (edges now))))
    (_ (error 'path-not-found))))

(defun %iter-edge (end open closed h now edges)
  (if (null edges)
      (%a*-rec end open closed h)
      (ematch edges
	((list* (and e (or (edge (eq now) neighbor)
			   (edge neighbor (eq now)))) rest)
	 (let ((old-cost (cost neighbor))
	       (cost ; f'(m)
		(+ (cost now) ; g*(n)
		   (cost e)   ; cost(n,m)
		   (heuristic-cost-between ; h*(m)
		    neighbor end))))
	   (cond
	     ((find neighbor (rb-member old-cost open))
	      (if (< cost old-cost) ; f'(m) < f*(m)
		  (progn
		    (setf (cost neighbor) cost
			  (parent neighbor) now)
		    (%iter-edge
		     end
		     (insert-queue cost neighbor
				   (remove-queue old-cost neighbor open))
		     closed h now rest))
		  (%iter-edge end open closed h now rest)))
	     
	     ((find neighbor (rb-member old-cost closed))
	      (if (< cost old-cost)
		  (progn
		    (setf (cost neighbor) cost
			  (parent neighbor) now)
		    (%iter-edge end
				(insert-queue cost neighbor open)
				(remove-queue old-cost neighbor closed)
				h now rest))
		  (%iter-edge end open closed h now rest)))

	     (t (setf (cost neighbor) cost)
		(setf (parent neighbor) now)
		(%iter-edge
		 end
		 (insert-queue cost neighbor open)
		 closed h now rest))))))))