(in-package :guicho-a-star-test)

(in-suite :a-star)

(defclass test-node (searchable-node)
  ((value :initarg :value :accessor value)
   (complementary-edge-class :initform 'test-edge)))

(defclass test-edge (searchable-edge)
  ((complementary-edge-class :initform 'test-node)))

(defmethod generic-eq ((n1 test-node) (n2 test-node))
  (= (value n1) (value n2)))

(defmethod heuristic-cost-between
    ((from test-node)
     (to test-node))
  (abs (- (value to) (value from))))

(defmethod generate-nodes ((n test-node))
  (list (make-instance 'test-node :value (1+ (value n)))))

(defmethod cost ((e test-edge))
  1)

(test stack-exhaust-check
  (let ((start (make-instance 'test-node :value 0))
        (end (make-instance 'test-node :value 1000000)))
    (a*-search-clos start end)))
