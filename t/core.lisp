

(in-package :aflab1-test)
(def-suite :core :in :aflab1)
(in-suite :core)

(test heuristic-cost-between
  (is (= (heuristic-cost-between
	  (2d 0 0)
	  (2d 3 4))
	 5.0)))

(test rb-tree
  (let ((tree (leaf)))
    (iter (for i from 0 below 50)
	  (setf tree (rb-insert tree i)))
    (is (= 0 (rb-minimum tree)))
    (is (= 49 (rb-maximum tree)))
    (iter (for i from 1 below 49)
	  (is (= (1- i)
		 (red-black-node-label
		  (rb-node-previous-node
		   (rb-member-node i tree) tree))))
	  (is (= (1+ i)
		 (red-black-node-label
		  (rb-node-next-node
		   (rb-member-node i tree) tree)))))))