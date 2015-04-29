(in-package :eazy-a*)
;; (DECLAIM (OPTIMIZE (DEBUG 1) (SAFETY 1) (SPACE 1) (SPEED 1)))
(cl-syntax:use-syntax :annot)

@export
@doc "
It requires following methods for the node classes are provided:

+ #'heuristic-cost-between
+ #'generic-eq
+ #'constraint-ordering-op
+ #'edges
+ #'cost
+ #'(setf cost)
+ #'(setf parent)

If goal-p-or-goal is a node, it uses #'generic-eq on the node
to determine the goal condition and
it uses #'heuristic-cost-between the current search node and the goal-p-or-goal
as the heuristic value of the node.

If goal-p-or-goal is a function, it uses the function as a goal condition,
and since no explicit information for a goal is provided, it requires
heuristic-fn to compute f*. If heuristic-fn is not provided, it uses (constantly
0), which means it actually runs a dijksrtra search."
(defun a*-search-clos (start goal-p-or-goal &key (verbose t) (heuristic-fn (constantly 0)))
  (declare (inline a*-search))
  (a*-search start
             (if (functionp goal-p-or-goal)
                 goal-p-or-goal
                 (curry #'generic-eq goal-p-or-goal))
             #'generic-eq
             #'edges
             (if (functionp goal-p-or-goal)
                 heuristic-fn
                 (rcurry #'heuristic-cost-between goal-p-or-goal))
             #'cost
             #'cost
             #'(setf cost)
             #'(setf parent)
             :verbose verbose
             :tiebreak (lambda (list)
                         (sort list #'< :key #'constraint-ordering-op))))
