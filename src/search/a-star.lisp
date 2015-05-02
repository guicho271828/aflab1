;;; a-star
(defpackage :eazy-a-star.search.a-star
  (:use :cl :ea*.b)
  (:shadowing-import-from :immutable-struct :defstruct)
  (:nicknames :ea*.s.a)
  (:export
   #:astar-node
   #:astar-node-g))
(in-package :ea*.s.a)

(defconstant +closed+ 1)
(defconstant +open+ 0)
(defconstant +neither+ -1)

(defstruct (astar-node (:include node))
  (g MOST-POSITIVE-FIXNUM :type fixnum)
  (f MOST-POSITIVE-FIXNUM :type fixnum)
  (status +neither+ :type (integer -1 1)))

(defpackage :eazy-a-star.search.a-star.eager
  (:use :cl :structure-interface :trivia
        :ea*.b :ea*.s.a :ea*.q)
  (:shadowing-import-from :immutable-struct :ftype)
  (:nicknames :ea*.s.a.e))
(in-package :ea*.s.a.e)

;;; eager a*

(implement-interface (ea*.s:search-interface))

(ftype expand t t (distance astar-node) (successor astar-node) (function (astar-node) (values)))
(defun expand (open closed h succ)
  (lambda (node)
    (delete-node open (aster-node-f node) node)
    (enqueue closed (aster-node-f node) node)
    (map nil
         (lambda-ematch
           ((edge :cost c
                  :to (and to (astar-node :f (place f f1)
                                          :g g
                                          :status status
                                          :parent (place p))))
            (let ((f2 (+ g c (funcall h to))))
              (match* (f2 status)
                ((_ +neither+)
                 (setf f f2
                       p node
                       status +open+)
                 (enqueue open f2 to))
                (((< f1) +open+)
                 (setf f f2
                       p node)
                 (delete-node open f1 to)
                 (enqueue open f2 to))
                (((< f1) +closed+)
                 (setf f f2
                       p node
                       status +open+)
                 (delete-node closed f1 to)
                 (enqueue open f2 to))))))
         (funcall succ node))
    (values)))

(defun fetch (open)
  (lambda ()
    (dequeue open)))


;;; lazy a*

;;; eager wa*

;;; lazy wa*
