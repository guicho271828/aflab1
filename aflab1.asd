#|
This file is a part of aflab1 project.
Copyright (c) 2013 guicho ()
|#

#|


Author: guicho ()
|#

(in-package :cl-user)
(defpackage aflab1-asd
  (:use :cl :asdf))
(in-package :aflab1-asd)

(defsystem aflab1
  :version "0.1"
  :author "guicho"
  :license "LLGPL"
  :depends-on (:iterate
		:optima
		:alexandria
		:cl-annot
		:guicho-utilities
                :guicho-utilities.threading
                :bordeaux-threads
		:cl-syntax-annot
		:anaphora)
  :components ((:module "src"
			:serial t
			:components
			((:file :package)
			 (:file :mixin)
			 (:file :rb-tree)
			 (:file :priority-queue)
			 (:file :a*-search)
			 
			 (:file :model)

			 (:module :8puzzle
				  :serial :t
				  :components
				  ((:file :model)
				   (:file :ops)))


			 )))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (load-op aflab1-test))))
