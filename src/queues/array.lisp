;;;; assume node-id

;;; list-bag
(defpackage :eazy-a-star.queue.array.list
  (:use :cl :ea*.b :ea*.q :trivia)
  (:shadowing-import-from :immutable-struct :ftype)
  (:nicknames :ea*.q.a.l)
  (:export #:init #:enqueue #:dequeue))
(in-package :ea*.q.a.l)

;; Assumes the histogram of the priority is very dense and the priority is
;; always an integer.
;; 
;; best for unit cost problems.

;; when not unit-cost, use gcd to try converting the costs?

;; (expt 2 26) array: up to 67108864, 400M
;; (expt 2 20) array: up to 1048576, 10M
;; (expt 2 18) array: up to 262144, 2.5M
;; array-dimension-limit
;; array-rank-limit
;; array-total-size-limit

(implement-interface (queue-methods (queue) init enqueue dequeue delete-node delete-id))
(defstruct queue
  (min 0 :type fixnum)
  (array (error "no array") :type (array list)))

(ftype reflesh-minimum queue (values))
(defun reflesh-minimum (queue)
  (ematch queue
    ((queue (min (place min)) array)
     (do () ((aref array min))
       (incf min))
     (values))))

(defun init (&optional (initial-max (expt 2 18)))
  (make-queue :array (make-array initial-max :element-type 'list :initial-element nil)))

(defun enqueue (queue node value)
  (ematch queue
    ((queue (min (place min)) array)
     (push node (aref array value))
     (alexandria:minf min value)
     (values))))

(defun dequeue (queue)
  (ematch queue
    ((queue (min (place min)) array)
     (if (aref array min)
         (prog1
           (values (pop (aref array min)) t)
           (reflesh-minimum queue))
         (values nil nil)))))

(defun delete-node (queue value node)
  (delete-id (node-id node)))

(defun delete-id (queue value id)
  (ematch queue
    ((queue array)
     (if (aref array value)
         (values (progn (setf (aref array value)
                              (delete id (aref array value) :key #'node-id))
                        (reflesh-minimum queue)
                        queue)
                 t)
         (values queue nil)))))

;;; hash-bag

(defpackage :eazy-a-star.queue.array.hash
  (:use :cl :ea*.b :ea*.q)
  (:shadowing-import-from :immutable-struct :ftype)
  (:nicknames :ea*.q.a.h)
  (:export #:init #:enqueue #:dequeue))
(in-package :ea*.q.a.h)

(ftype init &optional priority (array hashtable))
(defun init (&optional (initial-max (expt 2 18)))
  (make-array initial-max :element-type 'hashtable))

(ftype enqueue (array hashtable) t priority (values))
(defun enqueue (queue node value)
  (unless (aref queue value)
    (setf (aref queue value) (make-hash-table)))
  (setf (gethash (node-id node) (aref queue value)) node)
  (values))

(ftype dequeue (array hashtable) priority (values t boolean))
(defun dequeue (queue value)
  (if (aref queue value)
      (values (pop (aref queue value)) t)
      (values nil nil)))

;;; indexed rb-tree bag

(defpackage :eazy-a-star.queue.array.rb
  (:use :cl :trivialib.red-black-tree
        :ea*.b)
  (:shadowing-import-from :immutable-struct :ftype)
  (:nicknames :ea*.q.a.rb)
  (:export #:init #:enqueue #:dequeue))
(in-package :ea*.q.a.rb)

(defstruct queue (array (error "no array") :type (array rb-tree)))

(ftype init &optional priority queue)
(defun init (&optional (initial-max (expt 2 18)))
  (make-queue :array (make-array initial-max :element-type 'rb-tree :initial-element (leaf))))

(ftype enqueue (array rb-tree) t priority (values))
(defun enqueue (queue node value)
  (setf (aref (queue-array queue) value)
        (rb-insert (aref (queue-array queue) value) (index node) node))
  (values))

;; (ftype dequeue (array rb-tree) priority (values t boolean))
;; (defun dequeue (queue value)
;;   (ematch (aref queue value)
;;     ((leaf) (values nil nil))
;;     (it (values (rb-remove-minimum-node it) t))))

