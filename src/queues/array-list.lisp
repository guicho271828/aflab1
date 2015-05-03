(defpackage :eazy-a-star.queue.array.list
  (:use :cl :structure-interface :trivia
        :ea*.b :ea*.bag)
  (:shadowing-import-from :immutable-struct :ftype)
  (:shadow :delete-id :init)
  (:nicknames :ea*.q.a.l))
(in-package :ea*.q.a.l)

(defstruct queue
  (min #.array-dimension-limit :type (mod #.array-dimension-limit))
  (array (error "no array") :type (array ea*.bag.l:list-bag)))

(implement-interface (ea*.q:queue-interface queue))

(defun init (&optional (initial-max (expt 2 6)))
  (make-queue :min (1- initial-max)
              :array
              (let ((a (make-array initial-max
                                   :element-type 'ea*.bag.l:list-bag)))
                (dotimes (i initial-max a)
                  (setf (aref a i) (ea*.bag.l:init))))))


(ftype reflesh-minimum queue (values))
(defun reflesh-minimum (queue)
  (ematch queue
    ((queue (min (place min)) array)
     (do ((in-bound (array-in-bounds-p array min)
                    (array-in-bounds-p array min)))
         ((or (not in-bound)
              (not (emptyp (aref array min))))
          in-bound)
       (incf min))
     (values))))

(defun enqueue (queue value node)
  (ematch queue
    ((queue (min (place min)) array)
     (let ((value (min value (1- (length array)))))
       (insert (aref array value) node)
       (alexandria:minf min value)
       queue))))

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
