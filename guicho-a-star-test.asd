#|
  This file is a part of guicho-a-star project.
  Copyright (c) 2013 guicho ()
|#
(require :fiveam)
(in-package :cl-user)
(defpackage guicho-a-star-test-asd
  (:use :cl :asdf))
(in-package :guicho-a-star-test-asd)

(defsystem guicho-a-star-test
  :author "guicho"
  :license "LLGPL"
  :depends-on (:guicho-a-star
	       :fiveam
	       :vecto
	       :local-time
               :cl-test-more)
  :components ((:module "t"
                :serial t
                :components
                ((:file :package)
                 (:file :model)
		 (:file :drawer)
		 (:file :core)
		 (:file :a-star)
                 (:file :regression-stack-exhanst)
                 (:module :8puzzle
                          :serial :t
                          :components
                          ((:file :model)
                           (:file :ops)
                           (:file :test))))))
  :perform (load-op :after (op c) 
		    (eval (read-from-string "(fiveam:run! :guicho-a-star)"))
		    (asdf:clear-system c)))
