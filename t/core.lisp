

(in-package :aflab1-test)
(def-suite :core :in :aflab1)
(in-suite :core)

(test heuristic-cost-between
  (is (eq (heuristic-cost-between
	   (2d 0 0)
	   (2d 3 4))
	  5.0)))

(test findmin
  (is (= (findmin '(72 3 62 9 0 23 6 2 3 48 9)) 0))
  (is (eq (findmin nil) nil)))

(test heuristic-value-to
  (is (= (funcall 
	  (heuristic-value-to
	   (2d 10 10))
	  (tree 3.0 nil (2d 6 7)))
	 8.0)))