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
(defun a*-search (start end &key (verbose t))
  (if verbose
      (a*-search-verbose start end)
      (a*-search-silent start end)))

(defun a*-search-verbose (start end)
  (let ((*minimum-f* (heuristic-cost-between start end)))
    (format t "~%~w : opened f^*(n) = ~a" start *minimum-f*)
    (%a*-rec end
	     (rb-insert (leaf)
			 *minimum-f*
			; f*(s) = 0 + h*(s)
			(list start))
	     (leaf))))

;; fully tail-recursive

(defun %a*-rec (end open closed)
  (match (rb-minimum-node open)
    ((rb-node _ _ _ nil _)
     (%a*-rec end (rb-remove-minimum-node open) closed))
    ((rb-node _ _ f* list _)
     (destructuring-bind (now . rest)
	 (sort list #'< :key #'constraint-ordering-op)
       (if (generic-eq now end)
	   now
	   (progn
	     (when (< *minimum-f* f*)
	       (format t ", ~a" f*)
	       (setf *minimum-f* f*))
	     (%iter-edge end
			 (rb-insert open f* rest)
			 (insert-queue f* now closed)
			 now (edges now))))))
    (_ (error 'path-not-found))))

(defun %iter-edge (end open closed now edges)
  (if (null edges)
      (%a*-rec end open closed)
      (ematch edges
	((list* (and e (or (edge (eq now) neighbor)
			   (edge neighbor (eq now)))) rest)
	 (let* ((h (heuristic-cost-between neighbor end))
		(old-g* (cost neighbor))
		(old-f* (+ h old-g*))
		(new-g* (+ (cost now) (cost e)))
		(new-f* (+ h new-g*)))
	   (cond
	     ((find neighbor (rb-member old-f* open)
		    :test #'generic-eq)
	      (if (< new-g* old-g*) ; f'(m) < f*(m)
		  (progn
		    (setf (cost neighbor) new-g*
			  (parent neighbor) now)
		    (%iter-edge
		     end
		     (insert-queue
		      new-f* neighbor
		      (remove-queue
		       old-f* neighbor open))
		     closed now rest)
		    (%iter-edge end open closed now rest))))
	     
	     ((find neighbor (rb-member old-f* closed)
		    :test #'generic-eq)
	      (if (< new-g* old-g*)
		  (progn
		    (setf (cost neighbor) new-g*
			  (parent neighbor) now)
		    (let ()
		      (%iter-edge
		       end
		       (insert-queue new-f* neighbor open)
		       (remove-queue old-f* neighbor closed)
		       now rest)))
		  (%iter-edge end open closed now rest)))

	     (t (setf (cost neighbor) new-g*)
		(setf (parent neighbor) now)
		(%iter-edge
		 end
		 (insert-queue new-f* neighbor open)
		 closed now rest))))))))


(defun a*-search-silent (start end)
  (%a*-rec-silent end
		  (rb-insert (leaf)
			     (heuristic-cost-between start end)
			     ;; f*(s) = 0 + h*(s)
			     (list start))
		  (leaf)))


(defun %a*-rec-silent (end open closed)
  (match (rb-minimum-node open)
    ((rb-node _ _ _ nil _)
     (%a*-rec-silent end (rb-remove-minimum-node open) closed))
    ((rb-node _ _ f* list _)
     (destructuring-bind (now . rest)
	 (sort list #'< :key #'constraint-ordering-op)
       (if (generic-eq now end)
	   now
	   (%iter-edge-silent end
			      (rb-insert open f* rest)
			      (insert-queue f* now closed)
			      now (edges now)))))
    (_ (error 'path-not-found))))

(defun %iter-edge-silent (end open closed now edges)
  (if (null edges)
      (%a*-rec-silent end open closed)
      (ematch edges
	((list* (and e (or (edge (eq now) neighbor)
			   (edge neighbor (eq now)))) rest)
	 (let* ((h (heuristic-cost-between neighbor end))
		(old-g* (cost neighbor))
		(old-f* (+ h old-g*))
		(new-g* (+ (cost now) (cost e)))
		(new-f* (+ h new-g*)))
	   (cond
	     ((find neighbor (rb-member old-f* open)
		    :test #'generic-eq)
	      (if (< new-g* old-g*) ; f'(m) < f*(m)
		  (progn
		    (setf (cost neighbor) new-g*
			  (parent neighbor) now)
		    (%iter-edge-silent
		     end
		     (insert-queue
		      new-f* neighbor
		      (remove-queue
		       old-f* neighbor open))
		     closed now rest)
		    (%iter-edge-silent end open closed now rest))))
	     
	     ((find neighbor (rb-member old-f* closed)
		    :test #'generic-eq)
	      (if (< new-g* old-g*)
		  (progn
		    (setf (cost neighbor) new-g*
			  (parent neighbor) now)
		    (let ()
		      (%iter-edge-silent
		       end
		       (insert-queue new-f* neighbor open)
		       (remove-queue old-f* neighbor closed)
		       now rest)))
		  (%iter-edge-silent end open closed now rest)))

	     (t (setf (cost neighbor) new-g*)
		(setf (parent neighbor) now)
		(%iter-edge-silent
		 end
		 (insert-queue new-f* neighbor open)
		 closed now rest))))))))