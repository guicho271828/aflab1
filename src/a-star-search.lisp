(in-package :eazy-a*)
(speed*)
;; (optimize*)
(cl-syntax:use-syntax :annot)

(defvar *print-lock* (make-lock "IO Stream lock"))

@export
(define-condition path-not-found (condition)
  ()
  (:report
   (lambda (c s)
     (declare (ignore c))
     (declare (type stream s))
     (format s "a*-search: there is no possible path!"))))


@export 'solution

@export
(define-condition solution-found (condition)
  ((solution :initarg :solution :accessor solution)))

;; printers

(define-local-function %print-start (start)
  (when verbose
    (with-lock-held (*print-lock*)
      (format t
              "~2%~4tStart searching : ~w ~& Initial minf* = ~a"
              start minimum-f))))

(define-local-function %print-update-f* (f* open closed)
  (when verbose
    (when (< minimum-f f*)
      (with-lock-held (*print-lock*)
        (format t
                "~& thread ~x: minf* = ~a , open = ~a, closed = ~a"
                (position (current-thread) (all-threads))
                f* (queue-length open) (queue-length closed)))
      (setf minimum-f f*))))

(define-local-function %print-solution-found (open closed)
  (when verbose
    (with-lock-held (*print-lock*)
      (format t "~& Solution found! ~& Node expanded: ~a"
              (+ (queue-length open)
                 (queue-length closed))))))

(define-local-function %print-keep-searching ()
  (when verbose
    (with-lock-held (*print-lock*)
      (format t "~& Keep searching ..."))))

;;;; main definition
(progn
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
           (if tiebreak (sort list #'< :key tiebreak) list)
         (if (goal-p now)
             (progn
               (%print-solution-found open closed)
               (restart-case (progn (signal 'solution-found :solution now) now)
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

  (declaim (ftype (function (t &key 
                               (:verbose boolean)
                               (:test (equality t))
                               (:edges (function (t) list))
                               (:goal-p (predicate t))
                               (:h (function (t) (rational 0)))
                               (:c (function (t) (rational 0)))
                               (:g (function (t) (rational 0)))
                               (:set-g (function ((rational 0) t) (rational 0)))
                               (:set-parent (function (t t) t))
                               (:tiebreak (function (t) (rational 0)))) t)
                  a*-search))


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
  (defun a*-search (start
                    &key verbose
                      test edges goal-p
                      h c g set-g set-parent tiebreak)
    (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
    (declare (dynamic-extent verbose test edges goal-p h c g set-g set-parent tiebreak))
    (declare (type boolean verbose))
    (declare (type (equality t) test))
    (declare (type (function (t) list) edges))
    (declare (type (predicate t) goal-p))
    (declare (type (function (t) (rational 0)) h c g tiebreak))
    (declare (type (function ((rational 0) t) (rational 0)) set-g))
    (declare (type (function (t t) t) set-parent))
    (flet ((edges (c) (funcall edges c))
           (h (a) (funcall h a))
           (c (edge) (funcall c edge))
           (goal-p (a) (funcall goal-p a))
           ((setf g) (newval node) (funcall set-g newval node))
           (g (node) (funcall g node))
           ((setf parent) (newval node) (funcall set-parent newval node)))
      (declare (dynamic-extent #'edges #'h #'c #'g #'goal-p #'(setf g) #'(setf parent)))
      (declare (ftype (function (t) (rational 0)) g))
      (declare (ftype (function ((rational 0) t) t) (setf g)))
      (declare (ftype (function (t t) t)  (setf parent)))
      (declare (ftype (function ((rational 0) (or leaf red-black-node)) list) rb-member))
      (declare (inline g (setf g) (setf parent)))
      (let ((minimum-f 0))
        (more-labels () (%search
                         %rec %iter-edge
                         %print-start
                         %print-update-f*
                         %print-solution-found
                         %print-keep-searching)
          (declare (inline %search))
          (declare (dynamic-extent #'%search #'%rec #'%iter-edge))
          (%search start)))))
 (declaim (notinline a*-search))

  @export
  (defun a*-search-clos (start end &key (verbose t))
    (declare (inline a*-search))
    (a*-search
     start
     :verbose verbose
     :test #'generic-eq
     :edges #'edges
     :h (rcurry #'heuristic-cost-between end)
     :c #'cost
     :goal-p (curry #'generic-eq end)
     :g #'cost
     :set-g #'(setf cost)
     :set-parent #'(setf parent)
     :tiebreak #'constraint-ordering-op))

  (declaim (notinline a*-search-clos))) 
