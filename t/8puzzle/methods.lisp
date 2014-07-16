
(in-package :guicho-a-star-test)
(cl-syntax:use-syntax :annot)

(defmethod generic-eq ((n1 8puzzle-node) (n2 8puzzle-node))
  (equalp (state n1) (state n2)))

(defun verify (8p)
  (ematch 8p
    ((8puzzle state pos)
     (assert (= (aref state pos) 0))
     t)))

(defun 8puzzle (state pos &optional (class '8puzzle-node))
  (make-instance class :state state :position pos))



(defmethod print-object ((8p 8puzzle-node) s)
  (print-unreadable-object (8p s :type t)
    (match 8p
      ((and (8puzzle (and state (vector _ _ _ _ _ _ _ _ _)) _)
	    (node _ _ cost))
       (format s "~_~<~;~a ~a ~a~%~0:t~a ~a ~a~%~0:t~a ~a ~a~;~:> :cost ~a"
	       (coerce state 'list) cost)))))

