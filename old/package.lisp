
(in-package :cl-user)
(defpackage eazy-a-star-test
  (:use :cl
	:eazy-a*
	:guicho-red-black-tree
	:alexandria
	:iterate
	:vecto
	:guicho-utilities
	:local-time
	:optima
	:fiveam)
  (:shadow :rotate :fail))

(in-package :eazy-a-star-test)
(def-suite :eazy-a-star)


(let (my-start
      my-goal-specification
      (queue (ea*.q.a.l:init)))
  (forward-search my-start
                  (ea*.b:goalp my-goal-specification)
                  (ea*.s.ea:expand queue #'my-h #'my-succ)
                  (ea*.s.ea:fetch queue)))

