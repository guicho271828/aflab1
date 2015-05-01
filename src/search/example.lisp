(in-package :ea*)




(let (my-start
      my-goal-specification
      (queue (ea*.q.a:init)))
  (forward-search my-start
                  (my-goalp my-goal-specification)
                  (ea*.s.ea:expand queue
                                   #'ea*.q.a:enqueue
                                   #'h #'c #'g #'succ)
                  (eager-astar.fetcher queue #'array-queue-insert
                                       #'my-node-g #'my-tie-breaking)))
