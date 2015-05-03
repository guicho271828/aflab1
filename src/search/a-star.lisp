;;; a-star
(defpackage :eazy-a-star.search.a-star
  (:use :cl :ea*.b :structure-interface)
  (:nicknames :ea*.s.a)
  (:export
   #:a-star-node
   #:a-star-node-g
   #:a-star-node-f
   #:a-star-node-h
   #:a-star-node-status
   #:+closed+
   #:+open+
   #:+neither+))
(in-package :ea*.s.a)

(defconstant +closed+ 1)
(defconstant +open+ 0)
(defconstant +neither+ -1)

(defstruct (a-star-node (:include node))
  (g 0 :type fixnum)
  (f MOST-POSITIVE-FIXNUM :type fixnum)
  (h -1 :type fixnum)
  (status +neither+ :type (integer -1 1)))

;; (implement-interface
;;  (ea*.bag:bag-interface ea*.bag.h:hash-bag a-star-node)
;;  :inherit (ea*.bag.h:hash-bag node))
;; (implement-interface
;;  (ea*.bag:bag-interface ea*.bag.l:list-bag a-star-node)
;;  :inherit (ea*.bag.l:list-bag node))

(defpackage :eazy-a-star.search.a-star.eager
  (:use :cl :structure-interface :trivia
        :ea*.b :ea*.s.a :ea*.q)
  (:shadowing-import-from :immutable-struct :ftype)
  (:shadow :init)
  (:nicknames :ea*.s.a.e))
(in-package :ea*.s.a.e)

;;; eager a*

(implement-interface (ea*.s:search-interface a-star-node))

(defun expand (open closed heuristic succ)
  (lambda (node)
    (declare (notinline delete-node enqueue))
    (ematch node
      ((a-star-node :g g1 :f f1 :status (place status))
       (setf status +closed+)
       (delete-node open f1 node)
       (enqueue closed f1 node)
       (map nil
            (lambda-ematch
              ((edge :cost c :to to)
               (ematch to
                 ((a-star-node :f (place f)
                               :h (place h)
                               :g (place g)
                               :status (place status)
                               :parent (place p))
                  (when (minusp h) (setf h (funcall heuristic to)))
                  (let* ((g2 (+ g1 c)))
                    (match* (g2 status)
                      ((_ +neither+)
                       (setf f (+ g2 h)
                             g g2
                             p node
                             status +open+)
                       (enqueue open f to))
                      (((< g) +open+)
                       (delete-node open f to)
                       (setf f (+ g2 h)
                             g g2
                             p node)
                       (enqueue open f to))
                      (((< g) +closed+)
                       (delete-node closed f to)
                       (setf f (+ g2 h)
                             g g2
                             p node
                             status +open+)
                       (enqueue open f to))))))))
            (funcall succ node))))
    (values)))

(defun fetch (open)
  (lambda ()
    (dequeue open)))


;;; lazy a*

;;; eager wa*

;;; lazy wa*
