

(in-package :aflab1)
(cl-syntax:use-syntax :annot)
(declaim (inline swap-array))
(defun swap-array (pos1 pos2 ary)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  @type ary (svref fixnum 9)
  (let ((v1 (svref ary pos1)))
    (setf (svref ary pos1) (svref ary pos2)
	  (svref ary pos2) v1)))

(macrolet
    ((op (dir swap)
       `(defun ,dir (8p class)
	  (declare (optimize (speed 3) (safety 0) (debug 0)))
	  @type 8puzzle-node 8p
	  (ematch 8p
	    ((8puzzle state pos)
	     @type fixnum pos
	     @type state (svref fixnum 9)
	     (let ((st (copy-array state))
		   (np (+ pos ,swap)))
	       (swap-array pos np st)
	       (8puzzle st np class)))))))
  (op up -3)
  (op down 3)
  (op left -1)
  (op right 1))

@export
(defmethod generate-nodes (searchable-node))

(defmethod generate-nodes ((8p 8puzzle-node))
  (ematch 8p
    ((8puzzle _ pos)
     (symbol-macrolet ((class (class-of 8p))
		       (u (up 8p class))
		       (d (down 8p class))
		       (l (left 8p class))
		       (r (right 8p class)))
       (case pos
	 (0 (list r d))
	 (1 (list l r d))
	 (2 (list l d))
	 (3 (list u r d))
	 (4 (list u l r d))
	 (5 (list u l d))
	 (6 (list r u))
	 (7 (list l r u))
	 (8 (list l u)))))))
	 