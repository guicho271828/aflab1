(in-package :ea*)




(let (my-start
      my-goal-specification
      (queue (ea*.q.a.l:init)))
  (forward-search my-start
                  (my-goalp my-goal-specification)
                  (ea*.s.ea:expand queue #'my-h #'my-succ)
                  (ea*.s.ea:fetch queue)))


