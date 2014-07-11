#|
This file is a part of aflab1 project.
Copyright (c) 2013 guicho ()
|#

(in-package :aflab1-test)

(def-suite :a-star :in :aflab1)
(in-suite :a-star)

;; blah blah blah.

(defvar *max* 300 "The dimension of square field")

(defun make-samples (sample-num max)
  "make random samples"
  (iter (repeat sample-num)
        (collect (2d (random max) (random max)))))

(defun make-graph (samples average-edge-number)
  (iter outer
        (for 2d in samples)
        (iter (for i below (floor
                            (* average-edge-number
                               (+ 1 (gaussian-random -1 1)))))
              (in outer
                  (collect
                      (connect 2d (random-elt (remove 2d samples))))))))

(test test-with-draw
  (let* ((samples (make-samples 500 *max*))
         (edges (make-graph samples 10)))
    (with-canvas (:width *max* :height *max*)
      (set-rgba-stroke 0 0 0 0.3)
      (set-rgba-fill 0 0 0 0.3)
      (set-line-width 1)
      (mapc #'draw samples)
      (mapc #'draw edges)
      (unwind-protect
           (let* ((start (random-elt samples))
                  (end  (iter (for candidate = (random-elt samples))
                              (if (eq start candidate)
                                  (next-iteration)
                                  (return candidate))))
                  (last (a*-search start end)))
             (with-graphics-state
               (set-rgba-fill 1 0 0 0.7)
               (set-rgba-stroke 1 0 0 0.7)
               (draw start)
               (draw end)
               (set-line-width 2)
               (set-rgba-fill 1 0 0 0.5)
               (set-rgba-stroke 1 0 0 0.5)
               (draw-path last)))
        (ensure-directories-exist
         (asdf:system-relative-pathname
          :aflab1
          "result/"))
        (save-png (asdf:system-relative-pathname
                   :aflab1
                   (format nil "result/~a.png" (now))))))))

(defun benchmark ()
  (let* ((samples (make-samples 150 *max*))
         (edges (make-graph samples 7)))
    (format t "Number of samples: ~50A~&Number of edges: ~50A~&"
            (length samples) (length edges))
    (let* ((start (random-elt samples))
           (end (random-elt (remove start samples))))
      (time
       (%benchmark1 start end)))))

(defun %benchmark1 (start end)
  (declare (inline a*-search))
  (a*-search start end))

(defun %benchmark2 (start end)
  (declare (inline a*-search))
  (a*-search start end))

