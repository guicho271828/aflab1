#|
This file is a part of eazy-a-star project.
Copyright (c) 2013 guicho ()
|#

#|


Author: guicho ()
|#

(in-package :cl-user)
(defpackage eazy-a-star-asd
  (:use :cl :asdf))
(in-package :eazy-a-star-asd)

(defsystem eazy-a-star
  :version "0.1"
  :author "guicho"
  :license "LLGPL"
  :depends-on (:iterate
		:optima
		:alexandria
		:cl-annot
		:guicho-utilities
		:cl-syntax-annot)
  :components ((:module "src"
			:serial t
			:components
			((:file :package)
			 (:file :mixin)
                         (:file :specialized)
                         (:file :patterns)
			 (:file :rb-tree)
			 (:file :priority-queue)
			 (:file :a-star-search))))
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
  :in-order-to ((test-op (load-op eazy-a-star-test))))
