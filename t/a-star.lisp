#|
This file is a part of eazy-a-star project.
Copyright (c) 2013 guicho ()
|#

(in-package :eazy-a-star-test)

(def-suite :a-star :in :eazy-a-star)
(in-suite :a-star)

;; blah blah blah.

(defvar *max* 300 "The dimension of square field")

(defun make-samples (sample-num max)
  "make random samples"
  (iter (repeat sample-num)
        (collect (2d (random max) (random max)))))

(defun k-nearest (target k samples)
  (subseq (sort (remove target samples) #'<
                :key (curry #'heuristic-cost-between target))
          0 k))

(defun make-graph (samples average-edge-number)
  (iter outer
        (for 2d in samples)
        (iter (with connect-num = (floor
                                  (* average-edge-number
                                     (+ 1 (gaussian-random -1 1)))))
              (for next in
                   (shuffle
                    (k-nearest 2d (* 2 connect-num) samples)))
              (for i below connect-num)
              (in outer
                  (collect
                      (connect 2d next)))
              (in outer
                  (collect
                      (connect next 2d))))))

(defun top-left (samples)
  (iter (for s in samples)
        (finding s minimizing (heuristic-cost-between s (2d 0 0)))))
(defun bottom-right (samples)
  (iter (for s in samples)
        (finding s maximizing (heuristic-cost-between s (2d 0 0)))))

(test test-with-draw
  (let* ((samples (make-samples 500 *max*))
         (edges (make-graph samples 3)))
    (with-canvas (:width *max* :height *max*)
      (set-rgba-stroke 0 0 0 0.3)
      (set-rgba-fill 0 0 0 0.3)
      (set-line-width 1)
      (mapc #'draw samples)
      (mapc #'draw edges)
      (unwind-protect
           (let* ((start (top-left samples))
                  (end (bottom-right samples))
                  (last (a*-search-clos start end)))
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
          :eazy-a-star
          "result/"))
        (save-png (asdf:system-relative-pathname
                   :eazy-a-star
                   (format nil "result/~a.png" (now))))))))

(defparameter *samples* (make-samples 1000 *max*))
(defparameter *start* (top-left *samples*))
(defparameter *end* (bottom-right *samples*))

(defparameter *edges*
  (handler-bind ((path-not-found
                  (lambda (c)
                    (warn "no path! rebuilding the graph")
                    (invoke-restart (find-restart 'retry)))))
    (print "Doing something heavy ... please be patient")
    (do-restart ((retry (lambda (&rest args) (declare (ignore args)))))
      (dolist (s *samples*)
        (reinitialize-instance s :edges nil))
      (let ((edges (make-graph *samples* 3)))
        (a*-search-clos *start* *end*)
        edges))))

(defun benchmark ()
  (format t "Number of samples: ~50A~&Number of edges: ~50A~&"
          (length *samples*) (length *edges*))
  (dolist (s *samples*)
    (reinitialize-instance s))
  (time
   (a*-search-clos *start* *end* :verbose nil)))

