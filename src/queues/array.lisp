(defpackage :eazy-a-star.queue.array
  (:use :cl :structure-interface :trivia
        :ea*.b :ea*.bag)
  (:shadowing-import-from :immutable-struct :ftype)
  (:shadow :delete-id :init)
  (:nicknames :ea*.q.a))
(in-package :ea*.q.a)

(implement-interface (ea*.q:queue-interface queue-type t))

(function-cache:defcached queue-type (bag-type)
  (alexandria:with-gensyms (queue)
    (eval `(defstruct ,queue
             (min #.array-dimension-limit :type (mod #.array-dimension-limit))
             (array (error "no array") :type (array ,bag-type))))
    ,queue))

(deftype queue (bag) (queue-type bag))

(defun queue-constructor (bag-type)
  (alexandria:symbolicate 'make- (queue-type bag-type)))

(defun init (bag-type)
  (symbol-macrolet ((initial-max (expt 2 6)))
    (funcall (queue-constructor bag-type)
             :min (1- initial-max)
             :array
             (let ((a (make-array initial-max
                                  :element-type bag-type)))
               (dotimes (i initial-max a)
                 (setf (aref a i) (ea*.bag:init bag-type)))))))

(ftype reflesh-minimum queue boolean)
(defun reflesh-minimum (queue)
  (ematch queue
    ((queue (min (place min)) array)
     (do ((in-bound (array-in-bounds-p array min)
                    (array-in-bounds-p array min)))
         ((or (not in-bound)
              (not (emptyp (aref array min))))
          (return in-bound))
       (incf min)))))

(defun enqueue (queue value node)
  (ematch queue
    ((queue (min (place min)) (array (place array)))
     ;; update the length twice
     (unless (array-in-bounds-p array value)
       (let ((len (length array)))
         (setf array (adjust-array array (* 2 len)))
         (loop for i from len below (* 2 len)
               do (setf (aref a i) (ea*.bag.h:init)))))
     (insert (aref array value) node)
     (alexandria:minf min value)
     queue)))

(defun dequeue (queue)
  (ematch queue
    ((queue (min (place min)) array)
     (if (reflesh-minimum queue)
         (values (get1 (aref array min)) t)
         (values nil nil)))))

(defun delete-node (queue value node)
  (delete-id queue value (id node)))

(defun delete-id (queue value id)
  (ematch queue
    ((queue array)
     (let ((value (min value (1- (length array)))))
       (if (emptyp (aref array value))
           (values queue nil)
           (values (progn
                     (ea*.bag:delete-id (aref array value) id)
                     queue)
                   t))))))

