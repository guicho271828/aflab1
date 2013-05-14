

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
    ((tree _ parent content)
     (draw content)
     (draw-path parent))))