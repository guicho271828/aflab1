
(defpackage :eazy-a-star.queue.array.hash
  (:use :cl :ea*.b :ea*.bag :trivia)
  (:shadowing-import-from :immutable-struct :ftype)
  (:shadow :delete-id)
  (:nicknames :ea*.q.a.h))
(in-package :ea*.q.a.h)

(defvar +unbound+ '+unbound+)

(defstruct queue
  (min 0 :type fixnum)
  (array (error "no array") :type (array (or hash-table (eql +unbound+)))))

(implement-interface (ea*.q:queue-methods queue))

(ftype reflesh-minimum queue (values))
(defun reflesh-minimum (queue)
  (ematch queue
    ((queue (min (place min)) array)
     (do ((q (aref array min) (aref array min)))
         ((or (eq +unbound+ q)
              (emptyp q)))
       (incf min))
     (values))))

(defun init (&optional (initial-max (expt 2 18)))
  (make-queue :array (make-array initial-max :element-type 'hash-table :initial-element +unbound+)))

(defun enqueue (queue value node)
  (ematch queue
    ((queue (min (place min)) array)
     (let ((bag (aref array min)))
       (when (eq +unbound+ bag)
         (setf (aref array value) (make-hash-table)))
       (insert bag node)
       (alexandria:minf min value)
       (values)))))

(defun dequeue (queue)
  (ematch queue
    ((queue (min (place min)) array)
     (let ((bag (aref array min)))
       (if (eq +unbound+ bag)
           (values nil nil)
           (locally
               (declare (hash-table bag))
             (if (emptyp bag)
                 (values nil nil)
                 (prog1
                   (values (get1 bag) t)
                   (reflesh-minimum queue)))))))))

(defun delete-node (queue value node)
  (delete-id queue value (id node)))

(defun delete-id (queue value id)
  (ematch queue
    ((queue array)
     (if (typep (aref array value) 'hash-table)
         (values (progn (setf (aref array value)
                              (ea*.bag:delete-id (aref array value) id))
                        (reflesh-minimum queue)
                        queue)
                 t)
         (values queue nil)))))



;; (ftype init &optional priority (array hashtable))
;; (defun init (&optional (initial-max (expt 2 18)))
;;   (make-array initial-max :element-type 'hashtable))
;; 
;; (ftype enqueue (array hashtable) t priority (values))
;; (defun enqueue (queue node value)
;;   (unless (aref queue value)
;;     (setf (aref queue value) (make-hash-table)))
;;   (setf (gethash (id node) (aref queue value)) node)
;;   (values))
;; 
;; (ftype dequeue (array hashtable) priority (values t boolean))
;; (defun dequeue (queue value)
;;   (if (aref queue value)
;;       (values (pop (aref queue value)) t)
;;       (values nil nil)))

