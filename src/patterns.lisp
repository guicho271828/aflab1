
(in-package :guicho-a*)
(cl-syntax:use-syntax :annot)

(defpattern node (edges parent cost)
  `(class searchable-node
          (edges ,edges) (parent ,parent)
          (cost ,cost)))

(defpattern edge (from to)
  `(structure searchable-edge- (from ,from) (to ,to)))

(defpattern fixed-cost-edge (from to cost)
  `(structure fixed-cost-edge- (from ,from) (to ,to) (cost ,cost)))

(defpattern discrete-cost-edge (from to cost)
  `(structure discrete-cost-edge- (from ,from) (to ,to) (cost ,cost)))

(defpattern single-float-cost-edge (from to cost)
  `(structure single-float-cost-edge- (from ,from) (to ,to) (cost ,cost)))

(defpattern double-float-cost-edge (from to cost)
  `(structure double-float-cost-edge- (from ,from) (to ,to) (cost ,cost)))

(defpattern unit-cost-edge (from to)
  `(structure unit-cost-edge- (from ,from) (to ,to) (cost ,cost)))

