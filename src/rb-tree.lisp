(in-package :guicho-a*)
;; (speed*)
(optimize*)
(cl-syntax:use-syntax :annot)

; red-black-tree

(defstruct (leaf (:constructor leaf)))
(defstruct (red-black-node
	     (:constructor
	      rb-node
	      (color left label content right)))
  color
  left
  (label 0 :type number)
  content
  right)

(defmethod print-object ((o leaf) s)
  (format s "LEAF"))

(defmethod print-object ((o red-black-node) s)
  (match o
    ((rb-node color left label content right)
     (write (list color label content
		  left right) :stream s))))

(defpattern rb-node (color left label content right)
  `(red-black-node (color ,color)
		   (left,left)
		   (label ,label)
		   (content ,content)
		   (right ,right)))

(defun red (left label content right)
  (rb-node :red left label content right))
(defun black (left label content right)
  (rb-node :black left label content right))
(defpattern red (left label content right)
  `(rb-node :red ,left ,label ,content ,right))
(defpattern black (left label content right)
  `(rb-node :black ,left ,label ,content ,right))

(defun rb-member (x tree)
  (when-let ((node (rb-member-node x tree)))
    (red-black-node-content node)))

;; (defun (setf rb-member) (new-value x tree)
;;   (if-let ((node (rb-member-node x tree)))
;;     (setf (red-black-node-content node)
;; 	  new-value)
;;     (rb-insert tree x new-value)))

(defun rb-member-node (x tree)
  (match tree
    ((leaf) nil)
    ((rb-node _ left label _ right)
     (cond ((< x label) (rb-member-node x left))
           ((> x label) (rb-member-node x right))
           (t           tree)))))

(defun balance (tree)
  (match tree
    ((or (black (red (red a x xc b) y yc c) z zc d)
         (black (red a x xc (red b y yc c)) z zc d)
         (black a x xc (red (red b y yc c) z zc d))
         (black a x xc (red b y yc (red c z zc d))))
     (red (black a x xc b) y yc (black c z zc d)))
    (otherwise tree)))

(defun rb-minimum-node (tree)
  (match tree
    ((rb-node _ (leaf) _ _ _)
     tree)
    ((rb-node _ left _ _ _)
     (rb-minimum-node left))))

(defun rb-minimum (tree)
  (match (rb-minimum-node tree)
    ((rb-node _ _ label content _)
     (values content label))))

(defun rb-maximum-node (tree)
  (match tree
    ((rb-node _ _ _ _ (leaf))
     tree)
    ((rb-node _ _ _ _ right)
     (rb-maximum-node right))))

(defun rb-maximum (tree)
  (match (rb-maximum-node tree)
    ((rb-node _ _ label content _)
     (values content label))))

(defun rb-insert (tree x &optional (xc x))
  (labels ((ins (tree)
	     (match tree
	       ((leaf) (red (leaf) x xc (leaf)))
	       ((rb-node color left label content right)
		(cond
		  ((< x label)
		   (balance (rb-node color (ins left) label content right)))
		  ((> x label)
		   (balance (rb-node color left label content (ins right))))
		  (t (rb-node color left label xc right)))))))
    (match (ins tree)
      ((rb-node _ left label content right)
       (black left label content right)))))

(defun rb-remove-minimum-node (tree)
  (let (min-label min-content)
    (labels
	((rec (tree)
	   (match tree
	     ((leaf) tree)
	     ((rb-node _ (leaf) label content right)
	      (setf min-label label
		    min-content content)
	      right)
	     ((rb-node color left label content right)
	      (balance (rb-node color (rec left) label content right))))))
      (values (rec tree)
	      min-content
	      min-label))))

(defun rb-remove (tree x)
  (labels
      ((rec (tree)
	 (match tree
	   ((leaf) tree)
	   ((rb-node color left (guard y (= y x)) _ right)
	    (multiple-value-match (rb-remove-minimum-node right)
	      ((subtree content label)
	       (balance
		(rb-node color left label content subtree)))))
	   ((rb-node color left y content right)
	    (if (< x y)
		(balance (rb-node color (rec left) y content right))
		(balance (rb-node color left y content (rec right))))))))
    (match (rec tree)
      ((rb-node _ left y content right)
       (black left y content right)))))
