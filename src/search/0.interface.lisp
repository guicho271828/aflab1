(defpackage eazy-a-star.search
  (:use :cl :ea*.b :structure-interface)
  (:export :solution-not-found
           :solution-found
           :solution
           :search-condition
           :search-interface
           :forward-search)
  (:nicknames :ea*.s))

(in-package :ea*.s)

;;; conditions

(define-condition solution-not-found (error)
  ()
  (:report
   (lambda (c s)
     (declare (ignore c))
     (declare (type stream s))
     (format s "Solution not found!"))))

(define-condition solution-found ()
  ((solution :initarg :solution :accessor solution :type node)))

(define-condition search-condition () ())

(define-interface search-interface (open closed cost node edge)
  ((expand `(function (,open ,closed
                         (function (,node) ,cost)
                         (function (,node) (vector ,edge)))
                      (function (,node) (values))))
   (fetch  `(function (,open) (function () ,node))))
  :documentation "search engine interface")

;;; forward search

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
  ;; do not put the declaration here, it will suppress eldoc strings!
  ;; (locally
  ;;     (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  (do ((node start (funcall fetch)))
      ((when (funcall goalp node)
         (restart-case
             (progn (signal 'solution-found node) t)
           (continue () nil)))
       node)
    (unless node
      (error 'solution-not-found))
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
