

(in-package :aflab1-test)

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
    ((2d x y _)
     (arc x y 3 0 +2pi+)
     (fill-path))))

(defun draw-path (tree)
  (match tree
    ((tree _ 
	   (and parent (tree _ _ (2d x2 y2)))
	   (and c1 (2d x1 y1)))
     (draw c1)
     (move-to x1 y1)
     (line-to x2 y2)
     (stroke)
     (draw-path parent))))