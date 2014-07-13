(in-package :guicho-a*)
;; (speed*)
(optimize*)
(cl-syntax:use-syntax :annot)

(defvar *print-lock* (make-lock "IO Stream lock"))

@export '(a*-tree tree a*-tree-p a*-tree-cost
	  a*-tree-parent a*-tree-content)

@export
(define-condition path-not-found (condition)
  ()
  (:report
   (lambda (c s)
     @ignore c
     (format s "a*-search: there is no possible path!"))))


@export 'solution

@export
(define-condition solution-found (condition)
  ((solution :initarg :solution :accessor solution)))

(defvar *verbose* nil)
(defvar *minimum-f* nil
  "a value storing the minimum f value. It is used only for the verbosity.")

(progn
  (define-local-function %search (start)
    (let ((*minimum-f* (h start end)))
      (when *verbose*
        (with-lock-held (*print-lock*)
          (format t
                  "~2%~4tStart searching : ~w ~& Initial minf* = ~a"
                  start *minimum-f*)))
      (%rec (rb-insert (leaf)
                       *minimum-f*
                       ;; f*(s) = 0 + h*(s)
                       (list start))
            (leaf))))

  ;; fully tail-recursive

  (define-local-function %rec (open closed)
    (match (rb-minimum-node open)
      ((rb-node _ _ _ nil _)
       (%rec (rb-remove-minimum-node open) closed))
      ((rb-node _ _ f* list _)
       (destructuring-bind (now . rest)
           (sort list #'< :key tie-breaking)
         (when (test now end)
           (restart-return ((continue
                             (lambda ()
                               (when *verbose*
                                 (with-lock-held (*print-lock*)
                                   (format t "~& Keep searching ..."))))))
             (when *verbose*
               (with-lock-held (*print-lock*)
                 (format t "~& Solution found! ~& Node expanded: ~a"
                         (+ (queue-length open)
                            (queue-length closed)))))
             (signal 'solution-found :solution now)
             (return-from %rec now)))
         (when *verbose*
           (when (< *minimum-f* f*)
             (with-lock-held (*print-lock*)
               (format t
                       "~& thread ~x: minf* = ~a , open = ~a, closed = ~a"
                       (position (current-thread) (all-threads))
                       f* (queue-length open) (queue-length closed)))
             (setf *minimum-f* f*)))
         (%iter-edge (rb-insert open f* rest)
                     (insert-queue f* now closed)
                     now (edges now))))
      (_ (signal 'path-not-found))))

  (define-local-function %iter-edge (open closed now edges)
    (ematch edges
      ((list)
       (%rec open closed))
      ((list* (and e (or (edge (eq now) neighbor)
                         (edge neighbor (eq now)))) rest)
       (let* ((h (h neighbor end))
              (old-g* (g neighbor))
              (old-f* (+ h old-g*))
              (new-g* (+ (g now) (c e)))
              (new-f* (+ h new-g*)))
         (cond
           ((when-let ((member (rb-member old-f* open)))
              (find neighbor member :test test))
            (if (< new-g* old-g*) ; f'(m) < f*(m)
                (progn
                  (setf (g neighbor) new-g*
                        (parent neighbor) now)
                  (%iter-edge
                   (insert-queue
                    new-f* neighbor
                    (remove-queue
                     old-f* neighbor open))
                   closed now rest))
                (%iter-edge open closed now rest)))
           ;;
           ((when-let ((member (rb-member old-f* closed)))
              (find neighbor member :test test))
            (if (< new-g* old-g*)
                (progn
                  (setf (g neighbor) new-g*
                        (parent neighbor) now)
                  (%iter-edge
                   (insert-queue new-f* neighbor open)
                   (remove-queue old-f* neighbor closed)
                   now rest))
                (%iter-edge open closed now rest)))
           ;;
           (t (setf (g neighbor) new-g*)
              (setf (parent neighbor) now)
              (%iter-edge
               (insert-queue new-f* neighbor open)
               closed now rest)))))))

  @export
  @doc "Conduct an A* search. signals SOLUTION-FOUND (<< condition) when a solution is found,
 or PATH-NOT-FOUND (<< condition) otherwise. If the condition is not handled, it normally
 returns with the last node of the path, or nil when no solution was found.

The &key arguments are: verbose, test, h, c, g.
For h, c and g, if they are not provided, it assumes
the given arguments have the corresponding CLOS method
 `heuristic-cost-between node node', `cost edge', `cost node'.

 h : give a heuristic function. if h is not provided, it assumes the nodes
     has the corresponding CLOS method `heuristic-cost-between'.
 c : give a cost function for edges.
 g : give the current shortest path from the start to the node.

Condition `solution-found' is associated with  a `continue' restart, invoking of which
lets the search continue for another solution."
  (defun a*-search (start end
                    &key
                      (verbose t)
                      (test #'generic-eq)
                      (edges #'edges)
                      (h #'heuristic-cost-between)
                      (c #'cost)
                      (g #'cost)
                      (set-g #'(setf cost))
                      (parent #'parent)
                      (set-parent #'(setf parent))
                      (tie-breaking #'constraint-ordering-op))
    (let ((*verbose* verbose))
      (macrolet ((test (a b) `(funcall test ,a ,b))
                 (edges (c) `(funcall edges ,c))
                 (h (a b) `(funcall h ,a ,b))
                 (c (edge) `(funcall c ,edge)))
        (flet (((setf g) (newval node) (funcall set-g newval node))
               (g (node) (funcall g node))
               ((setf parent) (newval node) (funcall set-parent newval node))
               (parent (a) (funcall parent a)))
          (more-labels () (%search %rec %iter-edge)
            (%search start)))))))

