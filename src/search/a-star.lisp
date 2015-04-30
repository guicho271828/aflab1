;;; a-star
(defpackage :eazy-a-star.search.eager-a-star
  (:use :cl :ea*.e)
  (:shadowing-import-from :immutable-struct :ftype)
  (:nicknames :ea*.s.ea)
  (:export
   #:expand
   #:fetch))
(in-package :ea*.s.ea)

;;; eager a*

(defun expand (oc enq deq 
               node g h
               succ)
  (map nil
       (lambda (edge)
         (let ((f (+ (edge-cost edge)
                     (funcall h (edge-to edge))
                     (funcall g node))))
           
         )
       (funcall succ node)))

  (let ((successors ))
    (loop for (n . c) across successors
          for f = 
          do
       
    


  (funcall deq queue
  )

(defun fetch ()

  )



;;; lazy a*

;;; eager wa*

;;; lazy wa*
