(in-package :guicho-a-star-test)

(in-suite :a-star)

(defclass test-node (unit-cost-node) ())

(defmethod generic-eq ((n1 test-node) (n2 test-node))
  (= (cost n1) (cost n2)))

(defmethod heuristic-cost-between
    ((from test-node)
     (to test-node))
  (abs (- (cost to) (cost from))))

(defmethod generate-nodes ((n test-node))
  (list (make-instance 'test-node)))

(test stack-exhaust-check
  (let ((start (make-instance 'test-node))
        (end (make-instance 'test-node :cost 1000000)))
    (a*-search-clos start end)))
