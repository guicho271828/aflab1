

(in-package :aflab1-test)
(def-suite :core :in :aflab1)
(in-suite :core)

(test heuristic-cost-between
  (is (= (heuristic-cost-between
	  (2d 0 0)
	  (2d 3 4))
	 5.0)))
