#|
This file is a part of aflab1 project.
Copyright (c) 2013 guicho ()
|#

(in-package :aflab1-test)

(def-suite :a-star :in :aflab1)
(in-suite :a-star)

;; blah blah blah.

(defvar *max* 300)
(defparameter *sample-num* 55)
(defparameter *samples*
  (iter (repeat *sample-num*)
	(collecting (2d (random *max*) (random *max*)))))

(defparameter *edges* nil)

(test connect-neighbors
  (iter (for 2d in *samples*)
	(iter (generate i below (+ 2 (random 1)))
	      (for samples
		   initially *samples*
		   then (remove best samples))
	      (for best = 
		   (iter (for sample in samples)
			 (finding
			  sample
			  minimizing
			  (heuristic-cost-between 2d sample))))
	      (unless best
		(terminate))
	      (when (< (length (edges best)) (* 2 3))
		(push (connect 2d best) *edges*)
		(push (connect best 2d) *edges*)
		(next i))))
  (pass))

(defparameter *start* (random-elt *samples*))
(defparameter *end* 
  (iter (for candidate = (random-elt *samples*))
	(if (eq *start* candidate)
	    (next-iteration)
	    (return candidate))))

(test a*
  (finishes
    (with-canvas (:width *max* :height *max*)
      (set-rgba-stroke 0 0 0 0.3)
      (set-rgba-fill 0 0 0 0.3)
      (set-line-width 1)
      (mapcar #'draw *samples*)
      (mapcar #'draw *edges*)
      (unwind-protect
	  (let ((last (a*-search *start* *end*)))
	    (with-graphics-state
	      (set-rgba-fill 1 0 0 0.7)
	      (set-rgba-stroke 1 0 0 0.7)
	      (draw-path last)))
	(ensure-directories-exist
	 (asdf:system-relative-pathname
		   :aflab1
		   "result/"))
	(save-png (asdf:system-relative-pathname
		   :aflab1
		   (format nil "result/~a.png" (now))))))))
