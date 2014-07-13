

(in-package :guicho-a-star-test)

(defgeneric draw (thing))

(defmethod draw ((e 2d-edge))
  (ematch (edge-from e)
    ((2d x y)
     (move-to x y)))
  (ematch (edge-to e)
    ((2d x y)
     (line-to x y)))
  (stroke))

(defmethod draw ((n 2d-node))
  (match n
    ((2d x y)
     (arc x y 3 0 +2pi+)
     (fill-path))))

(defun draw-path (node)
  (match node
    ((and (2d x1 y1)
	  (node _ (and parent (2d x2 y2)) _))
     (move-to x1 y1)
     (line-to x2 y2)
     (stroke)
     (draw-path parent))))
