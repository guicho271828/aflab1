
(in-package :guicho-a*)
(cl-syntax:use-syntax :annot)

(defpattern node (edges parent cost)
  `(class searchable-node
          (edges ,edges) (parent ,parent)
          (cost ,cost)))

(defpattern edge (from to)
  `(structure searchable-edge- (from ,from) (to ,to)))
