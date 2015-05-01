(defpackage :eazy-a-star
  (:use :cl :ea*.b :ea*.q :ea*.s)
  (:nicknames :ea*)
  #.(let (syms)
      (dolist (p (mapcar #'find-package '(:ea*.b :ea*.q :ea*.s)))
        (do-external-symbols (s p)
          (push s syms)))
      `(:export ,@syms)))


