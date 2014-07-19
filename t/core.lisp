

(in-package :eazy-a-star-test)
(def-suite :core :in :eazy-a-star)
(in-suite :core)

(test heuristic-cost-between
  (is (= (heuristic-cost-between
	  (2d 0 0)
	  (2d 3 4))
	 5.0)))

(defparameter *tree* (leaf))

(iter (for i from 0 below 50)
      (setf *tree* (rb-insert *tree* i)))

(test rb-tree
  (iter (for i below 50)
	(is (= i (rb-member i *tree*))))
  (is (= 0 (rb-minimum *tree*)))
  (is (= 49 (rb-maximum *tree*)))
  (iter (for i from 1 below 49)
	(is (= (1- i)
	       (red-black-node-label
		(rb-node-previous-node
		 (rb-member-node i *tree*) *tree*))))
	(is (= (1+ i)
	       (red-black-node-label
		(rb-node-next-node
		 (rb-member-node i *tree*) *tree*))))))

(test rb-remove
  (iter (for i from 0 to 49)
	(is (= i (rb-maximum (rb-remove-after *tree* i)))))
  (iter (for i from 0 to 49)
	(is (= i (rb-minimum (rb-remove-before *tree* i))))))
