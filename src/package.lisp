#|
  This file is a part of eazy-a-star project.
  Copyright (c) 2013 guicho ()
|#

(in-package :cl-user)

(defpackage eazy-a-star
  (:use :cl
	:iterate
	:alexandria
	:trivia)
  (:export :solution-not-found
           :solution-found
           :solution)
  (:nicknames :eazy-a*))
(in-package :eazy-a-star)

;; blah blah blah.

