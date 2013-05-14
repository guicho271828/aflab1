#|
  This file is a part of aflab1 project.
  Copyright (c) 2013 guicho ()
|#

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
		 (:file :drawer)
		 (:file :core)
		 (:file "aflab1"))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
