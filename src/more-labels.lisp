(in-package :guicho-a*)
(cl-syntax:use-syntax :annot)


(defmacro define-local-function (name args &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get ',name :local-function) '(,args ,@body))
     (defun ,name ,args ,@body)))

(defmacro more-labels ((&key (inline nil)) definitions &body body)
  `(labels ,(mapcar (lambda (def)
                      (etypecase def
                        (symbol
                         (if inline
                             (get def :local-function)
                             (destructuring-bind (args . lbody)
                                 (get def :local-function)
                               @ignore lbody
                               (let ((args (mapcar (compose #'gensym #'symbol-name) args)))
                                 ;; use the global fdefinition intentionally
                                 `(,def ,args (funcall ',def ,@args))))))
                        (cons def)))
                    definitions)
     ,@body))
