(in-package :ea*)




(let (my-start
      my-goal-specification
      (queue (ea*.q.a.l:init)))
  (forward-search my-start
                  (my-goalp my-goal-specification)
                  (lambda ()
                  (ea*.s.ea:expand queue #'h #'c #'g #'succ)
                  (eager-astar.fetcher queue #'array-queue-insert
                                       #'my-node-g #'my-tie-breaking)))

06-6685-2414
