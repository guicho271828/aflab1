;;; a-star
(defpackage :eazy-a-star.search.a-star
  (:use :cl :ea*.b :structure-interface)
  (:nicknames :ea*.s.a)
  (:export
   #:a-star-node
   #:a-star-node-g
   #:a-star-node-f
   #:a-star-node-status
   #:+closed+
   #:+open+
   #:+neither+))
(in-package :ea*.s.a)

(defconstant +closed+ 1)
(defconstant +open+ 0)
(defconstant +neither+ -1)

(defstruct (a-star-node (:include node))
  (g MOST-POSITIVE-FIXNUM :type fixnum)
  (f MOST-POSITIVE-FIXNUM :type fixnum)
  (status +neither+ :type (integer -1 1)))

(implement-interface
 (ea*.bag:bag-interface hash-table a-star-node)
 :inherit (hash-table node))

(defpackage :eazy-a-star.search.a-star.eager
  (:use :cl :structure-interface :trivia
        :ea*.b :ea*.s.a :ea*.q)
  (:shadowing-import-from :immutable-struct :ftype)
  (:nicknames :ea*.s.a.e))
(in-package :ea*.s.a.e)

;;; eager a*

(implement-interface (ea*.s:search-interface a-star-node))

(defun expand (open closed h succ)
  (lambda (node)
    (declare (notinline delete-node enqueue))
    (delete-node open (a-star-node-f node) node)
    (enqueue closed (a-star-node-f node) node)
    (map nil
         (lambda-ematch
           ((edge :cost c
                  :to (and to (a-star-node :f (place f f1)
                                           :g g
                                           :status (place status)
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
