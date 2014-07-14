(in-package :guicho-red-black-tree)
(speed*)
;; (optimize*)
(cl-syntax:use-syntax :annot)

; red-black-tree

(defstruct (leaf (:constructor leaf)))

(defstruct (red-black-node
	     (:constructor
	      rb-node
	      (color left label content right)))
  (color :red :type symbol)
  left
  (label 0 :type rational)
  content
  right)

(defmethod print-object ((o leaf) s)
  (format s "LEAF"))

(defpattern rb-node (color left label content right)
  `(red-black-node (color ,color)
		   (left,left)
		   (label ,label)
		   (content ,content)
		   (right ,right)))

(defmethod print-object ((o red-black-node) s)
  (match o
    ((rb-node color left label content right)
     (write (list color label content
		  left right) :stream s))))

(declaim (ftype (function (t rational t t) red-black-node) red black))

(defun red (left label content right)
  (rb-node :red left label content right))
(defun black (left label content right)
  (rb-node :black left label content right))
(defpattern red (left label content right)
  `(rb-node :red ,left ,label ,content ,right))
(defpattern black (left label content right)
  `(rb-node :black ,left ,label ,content ,right))

(declaim (ftype (function (rational (or leaf red-black-node)) t) rb-member))
(declaim (ftype (function (rational (or leaf red-black-node)) (or null red-black-node)) rb-member-node))

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


(declaim (ftype (function ((or red-black-node leaf)) (or red-black-node leaf)) balance))
(defun balance (tree)
  (match tree
    ((or (black (red (red a x xc b) y yc c) z zc d)
         (black (red a x xc (red b y yc c)) z zc d)
         (black a x xc (red (red b y yc c) z zc d))
         (black a x xc (red b y yc (red c z zc d))))
     (red (black a x xc b) y yc (black c z zc d)))
    (otherwise tree)))

(declaim (ftype (function ((or red-black-node leaf)) (or null red-black-node))
                rb-minimum-node rb-maximum-node))
(declaim (ftype (function ((or red-black-node leaf)) t)
                rb-minimum rb-maximum))
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

(declaim (ftype (function ((or leaf red-black-node) rational &optional t) red-black-node) rb-insert))
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
    (declare (ftype (function ((or leaf red-black-node)) red-black-node) ins))
    (match (ins tree)
      ((rb-node _ left label content right)
       (black left label content right)))))

(declaim (ftype (function ((or leaf red-black-node)) (values (or leaf red-black-node) t rational))
                rb-remove-minimum-node))
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

(declaim (ftype (function ((or leaf red-black-node) rational) (or red-black-node leaf))
                rb-remove))
(defun rb-remove (tree x)
  (labels
      ((rec (tree)
	 (match tree
	   ((rb-node color left (guard y (= y x)) _ right)
	    (multiple-value-match (rb-remove-minimum-node right)
	      ((subtree content label)
	       (balance
		(rb-node color left label content subtree)))))
	   ((rb-node color left y content right)
	    (if (< x y)
                (if (typep left 'leaf)
                    (balance (rb-node color left y content right))
                    (balance (rb-node color (rec left) y content right)))
                (if (typep right 'leaf)
                    (balance (rb-node color left y content right))
                    (balance (rb-node color left y content (rec right)))))))))
    (declare (ftype (function (red-black-node) red-black-node) rec))
    (if (typep tree 'leaf)
        tree
        (match (rec tree)
          ((rb-node _ left y content right)
           (black left y content right))))))

(defun rb-node-next-node (node tree)
  (match node
    ((rb-node _ _ _ _ (and right (type red-black-node)))
     (rb-minimum-node right))
    (_ (%rb-node-next-node-rec node tree))))

(defun %rb-node-next-node-rec (node tree)
  (match tree
    ((leaf)
     tree)
    
    ((rb-node _ (guard left (eq (rb-maximum-node left) node)) _ _ _)
     tree)

    ((rb-node _ left label _ right)
     (match node
       ((rb-node _ _ x _ _)
	(cond ((< x label) (%rb-node-next-node-rec node left))
	      ((> x label) (%rb-node-next-node-rec node right))
	      (t right)))))))

(defun rb-node-previous-node (node tree)
  (match node
    ((rb-node _ (and left (type red-black-node)) _ _ _)
     (rb-maximum-node left))
    (_ (%rb-node-previous-node-rec node tree))))

(defun %rb-node-previous-node-rec (node tree)
  (match tree
    ((leaf)
     tree)
    
    ((rb-node _ _ _ _ (guard right (eq (rb-minimum-node right) node)))
     tree)

    ((rb-node _ left label _ right)
     (match node
       ((rb-node _ _ x _ _)
	(cond ((< x label) (%rb-node-previous-node-rec node left))
	      ((> x label) (%rb-node-previous-node-rec node right))
	      (t left)))))))

(defun rb-member-node-after (x tree)
  (match tree
    ((leaf) tree)
    ((rb-node _ left label _ right)
     (cond ((< x label)
	    (match (rb-maximum-node left)
	      ((rb-node _ _ y _ _)
	       (if (< y x)
		   tree
		   (rb-member-node-after x left)))
	      (_ tree)))
           ((< label x)
	    (match (rb-minimum-node right)
	      ((and min (rb-node _ _ y _ _))
	       (if (< x y)
		   min
		   (rb-member-node-after x right)))
	      (_ tree)))
           (t
	    (rb-minimum-node right))))))

(defun rb-member-node-before (x tree)
  (match tree
    ((leaf) tree)
    ((rb-node _ left label _ right)
     (cond ((< x label)
	    (match (rb-maximum-node left)
	      ((and max (rb-node _ _ y _ _))
	       (if (< y x)
		   max
		   (rb-member-node-before x left)))
	      (_ tree)))
           ((< label x)
	    (match (rb-minimum-node right)
	      ((rb-node _ _ y _ _)
	       (if (< x y)
		   tree
		   (rb-member-node-before x right)))
	      (_ tree)))
           (t
	    (rb-maximum-node left))))))
(defun rb-member-after (x tree)
  (red-black-node-content
   (rb-member-node-after x tree)))
(defun rb-member-before (x tree)
  (red-black-node-content
   (rb-member-node-before x tree)))

(defun rb-remove-after (tree x)
  (match tree
    ((leaf) tree)
    ((rb-node color left label content right)
     (cond ((< x label)
	    (match (rb-maximum-node left)
	      ((rb-node _ _ y _ _)
	       (if (< y x)
		   left
		   (rb-remove-after left x)))
	      (_ (leaf))))
           ((< label x)
	    (match (rb-minimum-node right)
	      ((rb-node _ _ y _ _)
	       (if (< x y)
		   (balance (rb-node color left label content (leaf)))
		   (balance (rb-node color left label content 
				     (rb-remove-after right x)))))
	      (_ tree)))
           (t
	    (balance (rb-node color left label content (leaf))))))))

(defun rb-remove-before (tree x)
  (match tree
    ((leaf) tree)
    ((rb-node color left label content right)
     (cond ((< x label)
	    (match (rb-maximum-node left)
	      ((rb-node _ _ y _ _)
	       (if (< y x)
		   (balance (rb-node color (leaf) label content right))
		   (balance (rb-node color (rb-remove-before left x)
				     label content right))))
	      (_ (leaf))))
           ((< label x)
	    (match (rb-minimum-node right)
	      ((rb-node _ _ y _ _)
	       (if (< x y)
		   right
		   (rb-remove-before right x)))
	      (_ tree)))
           (t
	    (balance (rb-node color (leaf) label content right)))))))

