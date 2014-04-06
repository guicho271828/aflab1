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
(define-condition path-not-found (condition)
  ()
  (:report
   (lambda (c s)
     @ignore c
     (format s "a*-search: there is no possible path!"))))

(defvar *minimum-f* nil)

@export
@doc "Conduct an A* search. signals solution-found (<< condition) when a solution is found,
 or path-not-found (<< condition) otherwise. If the condition is not handled it normally
 returns with the last node of the path, or nil when no solution was found.

Condition `solution-found' is associated with  a `continue' restart, invoking of which
lets the search continue for another solution."
(defun a*-search (start end &key (verbose t))
  (if verbose
      (a*-search-verbose start end)
      (a*-search-silent start end)))

(defun a*-search-verbose (start end)
  (let ((*minimum-f* (heuristic-cost-between start end)))
    (with-lock-held (*print-lock*)
      (format *shared-output*
              "~2%~4tStart searching : ~w ~& minf* = ~a"
              start *minimum-f*))
    (%a*-rec end
	     (rb-insert (leaf)
			 *minimum-f*
			; f*(s) = 0 + h*(s)
			(list start))
	     (leaf))))

;; fully tail-recursive

@export 'solution

@export
(define-condition solution-found (condition)
  ((solution :initarg :solution :accessor solution)))

(defun %a*-rec (end open closed)
  (match (rb-minimum-node open)
    ((rb-node _ _ _ nil _)
     (%a*-rec end (rb-remove-minimum-node open) closed))
    ((rb-node _ _ f* list _)
     (destructuring-bind (now . rest)
	 (sort list #'< :key #'constraint-ordering-op)
       (when (generic-eq now end)
         (restart-return ((continue
                           (lambda ()
                             (with-lock-held (*print-lock*)
                               (format *shared-output* "~& Keep searching ...")))))
           (with-lock-held (*print-lock*)
             (format *shared-output* "~& Solution found!"))
           (signal 'solution-found :solution now)
           (return-from %a*-rec now)))
       (when (< *minimum-f* f*)
         (with-lock-held (*print-lock*)
           (format *shared-output*
                   "~& thread ~x: minf* = ~a"
                   (position (current-thread) (all-threads)) f*))
         (setf *minimum-f* f*))
       (%iter-edge end
                   (rb-insert open f* rest)
                   (insert-queue f* now closed)
                   now (edges now))))
    (_ (signal 'path-not-found))))

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
		     closed now rest))
		  (%iter-edge end open closed now rest)))
	     
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
       (when (generic-eq now end)
         (restart-return ((continue
                           (lambda ()
                             nil)))
           (signal 'solution-found :solution now)
           (return-from %a*-rec-silent now)))
       (%iter-edge-silent end
                          (rb-insert open f* rest)
                          (insert-queue f* now closed)
                          now (edges now))))
    (_ (signal 'path-not-found))))

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
		     closed now rest))
		  (%iter-edge-silent end open closed now rest)))
	     
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