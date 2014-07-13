#|
  This file is a part of aflab1 project.
  Copyright (c) 2013 guicho ()
|#
(require :fiveam)
(in-package :cl-user)
(defpackage aflab1-test-asd
  (:use :cl :asdf))
(in-package :aflab1-test-asd)

(defsystem aflab1-test
  :author "guicho"
  :license "LLGPL"
  :depends-on (:aflab1
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
		    (eval (read-from-string "(fiveam:run! :aflab1)"))
		    (asdf:clear-system c)))
