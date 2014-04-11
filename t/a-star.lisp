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
(defparameter *samples* nil)
(defparameter *edges* nil)
(defparameter *max-edges-per-node* 3)

(defun make-samples ()
  (setf *samples* nil)
  (iter (repeat *sample-num*)
        (push (2d (random *max*) (random *max*))
              *samples*)))

(defun connect-neighbors ()
  (setf *edges* nil)
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
              (when (< (length (edges best)) *max-edges-per-node*)
                (push (connect 2d best) *edges*)
                (next i)))))

(test a*
  (make-samples)
  (connect-neighbors)
  (finishes
    (with-canvas (:width *max* :height *max*)
      (set-rgba-stroke 0 0 0 0.3)
      (set-rgba-fill 0 0 0 0.3)
      (set-line-width 1)
      (mapcar #'draw *samples*)
      (mapcar #'draw *edges*)
      (unwind-protect
          (let* ((start (random-elt *samples*))
                 (end  (iter (for candidate = (random-elt *samples*))
                             (if (eq start candidate)
                                 (next-iteration)
                                 (return candidate))))
                 (last (a*-search start end)))
            (with-graphics-state
              (set-rgba-fill 1 0 0 0.7)
              (set-rgba-stroke 1 0 0 0.7)
              (draw start)
              (draw end)
              (set-rgba-fill 1 1 0 0.7)
              (set-rgba-stroke 1 1 0 0.7)
              (draw-path last)))
	(ensure-directories-exist
	 (asdf:system-relative-pathname
	  :aflab1
	  "result/"))
	(save-png (asdf:system-relative-pathname
		   :aflab1
		   (format nil "result/~a.png" (now))))))))

(defun benchmark ()
  (let ((*max* 300)
        (*sample-num* 55)
        (*max-edges-per-node* 7))
    (make-samples)
    (connect-neighbors)
    (let* ((start (random-elt *samples*))
           (end  (iter (for candidate = (random-elt *samples*))
                       (if (eq start candidate)
                           (next-iteration)
                           (return candidate)))))
      (time
       (%benchmark1 start end))
      (time
       (%benchmark2 start end)))))

(defun %benchmark (start end)
  (declare (inline a*-search))
  (a*-search start end))

(defun %benchmark (start end)
  (declare (inline a*-search))
  (a*-search start end))

