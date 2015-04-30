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
  (:shadowing-import-from :immutable-struct :ftype :defstruct)
  (:export :solution-not-found
           :solution-found
           :solution)
  (:nicknames :ea*))

