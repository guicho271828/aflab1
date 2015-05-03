

(in-package :cl-user)
(defpackage eazy-a-star-test
  (:use :cl
	:ea*
	:alexandria
	:iterate
        :trivia
	:vecto
	:local-time
        :fiveam
        :structure-interface)
  (:shadowing-import-from :immutable-struct :ftype)
  (:shadow :goalp)
  (:shadow :rotate))

(in-package :eazy-a-star-test)
(def-suite :eazy-a-star)
(in-suite :eazy-a-star)
(defvar *max* 300 "The dimension of square field")
(defvar *sample-num* 10)
(defvar *samples*)
(defvar *edge-number* 5)
(defconstant +2pi+ (* 2 PI))

(defstruct (2d-node (:include eazy-a-star.search.a-star:a-star-node))
  (x 0 :type fixnum)
  (y 0 :type fixnum))

(defun make-samples (&optional (*sample-num* *sample-num*))
  "make random samples"
  (iter (repeat *sample-num*)
        (for i = (make-2d-node :x (random *max*) :y (random *max*)))
        ;;(initialize-instance i :parent i :x (random *max*) :y (random *max*))
        (collect i)))

(defun ^2 (x) (* x x))

(defun distance (from to)
  (ematch to
    ((2d-node :x x1 :y y2)
     (ematch from
       ((2d-node :x x2 :y y2)
        (sqrt (+ (^2 (- x2 x1)) (^2 (- y2 y1)))))))))

(defun draw2 (from to)
  (ematch from
    ((2d-node x y)
     (move-to x y)))
  (ematch to
    ((2d-node x y)
     (line-to x y)))
  (stroke))

(defun draw (n)
  (match n
    ((2d-node x y)
     (arc x y 3 0 +2pi+)
     (fill-path))))

(defun draw-path (node)
  (match node
    ((2d-node (parent (eq *default-parent-node*)))
     nil)
    ((2d-node parent)
     (draw2 node parent)
     (draw-path parent))))

(defun k-nearest (target k distance samples)
  (subseq (sort (mapcar (lambda (s) (cons (funcall distance target s) s))
                        (remove target samples))
                #'<
                :key #'car)
          0 k))

(defun my-succ (node)
  (map 'vector
       (lambda-match
         ((cons dist sample)
          (make-edge :cost dist :to sample)))
       (k-nearest node *edge-number* #'distance *samples*)))

;; (implement-interface (ea*.s:goalp-interface 2d-node))

(defun goalp (goal)
  (lambda (node)
    (eq goal node)))

(sb-profile:profile astar-search ea*.s:forward-search ea*.s:expand
                    ea*.s:fetch ea*.q::delete-node ea*.q::enqueue)

(ftype astar-search 2d-node 2d-node 2d-node)
(defun astar-search (start goal)
  (declare (notinline ea*.s:forward-search
                      ea*.s:expand
                      ea*.s:fetch
                      ea*.q::delete-node
                      ea*.q::enqueue))
  (let ((open (ea*.q.a.l:init))
        (closed (ea*.q.a.l:init)))
    (ea*.s:forward-search
     start
     (goalp goal)
     (ea*.s:expand open closed #'distance #'my-succ)
     (ea*.s:fetch open))))

(defun call-with-drawing (fn)
  (with-canvas (:width *max* :height *max*)
    (set-rgba-stroke 0 0 0 0.3)
    (set-rgba-fill 0 0 0 0.3)
    (set-line-width 1)
    (mapc #'draw *samples*)
    (unwind-protect (funcall fn)
      (ensure-directories-exist
       (asdf:system-relative-pathname
        :eazy-a-star
        "result/"))
      (save-png (asdf:system-relative-pathname
                 :eazy-a-star
                 (format nil "result/~a.png" (now)))))))

(test astar
  (let ((*sample-num* 50)
        (*samples* (make-samples)))
    (format t "Number of samples: ~50A~&" *sample-num*)
    (finishes
     (call-with-drawing
      (lambda ()
        (let ((start (random-elt *samples*))
              (goal (random-elt *samples*)))
          (set-rgba-fill 1 0 0 0.7)
          (set-rgba-stroke 1 0 0 0.7)
          (draw start)
          (draw goal)
          (set-line-width 2)
          (set-rgba-fill 1 0 0 0.5)
          (set-rgba-stroke 1 0 0 0.5)
          (draw-path (astar-search start goal))))))))



