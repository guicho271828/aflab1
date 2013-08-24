(in-package :guicho-a*)
(speed*)
;; (optimize*)
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
(define-condition path-not-found (error)
  ()
  (:report
   (lambda (c s)
     @ignore c
     (format s "a*-search: there is no possible path!"))))

(defvar *minimum-f* nil)

@export
(defun a*-search (start end)
  (declare (optimize (debug 3)))
  (let ((*minimum-f* MOST-NEGATIVE-DOUBLE-FLOAT))
    (%a*-rec end
	     (rb-insert (leaf)
			(heuristic-cost-between start end)
			; f*(s) = 0 + h*(s)
			(list start))
	     (leaf))))

;; fully tail-recursive

(defun %a*-rec (end open closed)
  (match (rb-minimum-node open)
    ((rb-node _ _ _ nil _)
     (%a*-rec end (rb-remove-minimum-node open) closed))
    ((rb-node _ _ cost (list* now rest) _)
     (when (< *minimum-f* cost)
       (format t "~%opened f^*(n) = ~a" cost)
       (setf *minimum-f* cost))
     (if (generic-eq now end)
	 now
	 (%iter-edge end
		     (rb-insert open cost rest)
		     (insert-queue cost now closed)
		     now (edges now))))
    (_ (error 'path-not-found))))

(defun %iter-edge (end open closed now edges)
  (if (null edges)
      (%a*-rec end open closed)
      (ematch edges
	((list* (and e (or (edge (eq now) neighbor)
			   (edge neighbor (eq now)))) rest)
	 (let* ((h (heuristic-cost-between neighbor end))
					; h*(m)
		(old-cost (+ (cost neighbor) h))
					; f*(m) = g*(m) + h*(m)
		(cost (+ (cost now) (cost e) h)))
					; f'(m) = g*(n) + cost(n,m) + h*(m)
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
		     closed now rest))
		  (%iter-edge end open closed now rest)))
	     
	     ((find neighbor (rb-member old-cost closed))
	      (if (< cost old-cost)
		  (progn
		    (setf (cost neighbor) cost
			  (parent neighbor) now)
		    (%iter-edge end
				(insert-queue cost neighbor open)
				(remove-queue old-cost neighbor closed)
				now rest))
		  (%iter-edge end open closed now rest)))

	     (t (setf (cost neighbor) cost)
		(setf (parent neighbor) now)
		(%iter-edge
		 end
		 (insert-queue cost neighbor open)
		 closed now rest))))))))