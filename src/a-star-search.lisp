(in-package :eazy-a*)
;; (DECLAIM (OPTIMIZE (DEBUG 1) (SAFETY 1) (SPACE 1) (SPEED 1)))
(cl-syntax:use-syntax :annot)

(define-condition path-not-found (condition)
  ()
  (:report
   (lambda (c s)
     (declare (ignore c))
     (declare (type stream s))
     (format s "a*-search: there is no possible path!"))))

(define-condition solution-found (condition)
  ((solution :initarg :solution :accessor solution)))

;;; printers
(define-local-function %print-start (start)
  (when verbose
    (format t
            "~2%~4tStart searching : ~w ~& Initial minf* = ~a"
            start minimum-f)))

(define-local-function %print-update-f* (f* open closed)
  (when verbose
    (when (< minimum-f f*)
      (format t "~& minf* = ~a , open = ~a, closed = ~a"
              f* (queue-length open) (queue-length closed))
      (setf minimum-f f*))))

(define-local-function %print-solution-found (open closed)
  (when verbose
    (format t "~& Solution found! ~& Node expanded: ~a"
            (+ (queue-length open)
               (queue-length closed)))))

(define-local-function %print-keep-searching ()
  (when verbose
    (format t "~& Keep searching ...")))

;;; main definition

(define-local-function %search (start)
  (setf minimum-f (h start))
  (%print-start start)
  ;; f*(s) = 0 + h*(s)
  (%rec (rb-insert (leaf) minimum-f (list start))
        (leaf)))

(define-local-function %rec (open closed)
  (match (rb-minimum-node open)
         ((rb-node _ _ _ nil _)
          (%rec (rb-remove-minimum-node open) closed))
         ((rb-node _ _ f* list _)
          (%print-update-f* f* open closed)
          (destructuring-bind (now . rest)
              (if tiebreak (funcall tiebreak list) list)
            (if (goal-p now)
                (progn
                  (%print-solution-found open closed)
                  (restart-case
                      (progn
                        (signal 'solution-found :solution now)
                        (values now f*))
                    (continue ()
                      (%print-keep-searching)
                      (%iter-edge (rb-insert open f* rest)
                                  (insert-queue f* now closed)
                                  now (edges now)))))
                (%iter-edge (rb-insert open f* rest)
                            (insert-queue f* now closed)
                            now (edges now)))))
         (_ (signal 'path-not-found))))

(define-local-function %iter-edge (open closed now edges)
  (ematch edges
          ((list)
           (%rec open closed))
          ((list* (and e (or (edge (eq now) neighbor)
                             (edge neighbor (eq now)))) rest)
           (let* ((h (h neighbor))
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

(declaim (inline a*-search a*-search-clos))

(deftype predicate (&optional (arg t))
  `(function (,arg) boolean))
(deftype equality (&optional (arg t))
  `(function (,arg ,arg) boolean))

(declaim (ftype (function (t                   ; start
                           (predicate t)       ; goal-p
                           (equality t)        ; test
                           (function (t) list) ; edges
                           (function (t) (real 0)) ; h
                           (function (t) (real 0)) ; c
                           (function (t) (real 0)) ; g
                           (function ((real 0) t) (real 0)) ; set-g
                           (function (t t) t) ;set-parent
                           &key
                           (:verbose boolean)
                           (:tiebreak (or null (function (list) list))))
                          (values t &optional (real 0)))
                a*-search))


@export
(defun a*-search (start goal-p test edges
                  h c g set-g set-parent
                  &key verbose tiebreak)
  (declare (dynamic-extent verbose test edges goal-p h c g set-g set-parent tiebreak))
  (locally
      (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
    (flet ((goal-p (a) (funcall goal-p a))
           (edges (c) (funcall edges c))
           (h (a) (funcall h a))
           (c (edge) (funcall c edge))
           (g (node) (funcall g node))
           ((setf g) (newval node) (funcall set-g newval node))
           ((setf parent) (newval node) (funcall set-parent newval node)))
      (declare (dynamic-extent #'edges #'h #'c #'g #'goal-p #'(setf g) #'(setf parent)))
      (declare (ftype (function ((real 0) rb-tree) list) rb-member))
      (declare (inline goal-p edges h c g (setf g) (setf parent))) 
      (let ((minimum-f 0))
        (more-labels () (%search
                         %rec %iter-edge
                         %print-start
                         %print-update-f*
                         %print-solution-found
                         %print-keep-searching)
                     (declare (inline %search))
                     (declare (dynamic-extent #'%search #'%rec #'%iter-edge))
                     (%search start))))))
(declaim (notinline a*-search))


(setf (documentation 'a*-search 'function)
      "
Conduct an A* search. signals SOLUTION-FOUND when a solution is found,
 or PATH-NOT-FOUND otherwise. If the condition is not handled, it normally
 returns with the last node of the path, or nil when no solution was found.

Condition `solution-found' is associated with  a `continue' restart, invoking of which
lets the search continue for another solution.

The arguments:

+ start :: The initial search node.
+ verbose :: if non-NIL print various information (e.g. the current best f*) during the search.
+ test :: binary function returning boolean for testing the node equality.
+ edges :: node -> (list edge). If the edges are not yet
           instantiated, it should generate them.
+ goal-p :: node -> boolean. =a*-search= signals SOLUTION-FOUND if it is satisfied by some node.
+ h :: node -> (real 0). Computes a heuristic value for a node.
+ c :: edge -> (real 0), reader function for an edge. Returns a cost function for edges.
+ g :: node -> (real 0), reader function for a node. Returns the current
       shortest path from the start to the node.
+ (setf set-g) :: (real 0), node -> (real 0), writer function for a node.
                  Set the current shortest path from the start to the node.
+ (setf set-parent) :: node n1, node n2 -> node n1, writer function for a node n2.
     Set the neighbor node n1 that yields the current shortest path as a parent node
     of n2.
+ tiebreak :: NIL, or (list node) -> (list node).
              If provided, sort the list of nodes of the same f*
              values. It may destructively modify the given list.
")
