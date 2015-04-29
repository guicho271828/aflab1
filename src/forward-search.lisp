

;;; generic forward search
(in-package :eazy-a*)

;;; conditions

(define-condition solution-not-found (error)
  ()
  (:report
   (lambda (c s)
     (declare (ignore c))
     (declare (type stream s))
     (format s "Solution not found!"))))

(define-condition solution-found ()
  ((goal :initarg :goal :accessor goal)))

(define-condition search-condition () ())

;;; type definition

(declaim (ftype (function (node                   ; start
                           (predicate node)       ; goalp
                           (function (node) (values)) ; expand
                           (function () node) ; fetch
                           &key
                           (:verbose t))
                          (values node))
                forward-search))

;;; main definition

(declaim (inline forward-search))
(defun forward-search (start goalp expand fetch)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (do ((node start (funcall fetch)))
      ((when (goalp node)
         (restart-case
             (progn (signal 'solution-found node) t)
           (continue () nil)))
       node)
    (funcall expand node)))
(declaim (notinline forward-search))

;;; documentation

(setf (documentation 'forward-search 'function)
      "
Conduct a heuristic forward-search. Signals SOLUTION-FOUND when a solution is found.
Condition `solution-found' is associated with a `continue' restart, invoking of which
lets the search continue for another solution.
arguments:

+ start :: The initial search node.
+ expand :: node -> (values).
+ goalp :: node -> boolean. =forward-search= signals SOLUTION-FOUND if it is satisfied by some node.
+ fetch :: () -> node. ")
