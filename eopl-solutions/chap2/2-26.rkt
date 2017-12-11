#lang eopl

(require "base.ss")

;; Red-blue-tree    ::= Red-blue-subtree
;; Red-blue-subtree ::= (red-node Red-blue-subtree Red-blue-subtree)
;;                  ::= (blue-node {Red-blue-subtree}*)
;;                  ::= (leaf-node Int)
(define-datatype red-blue-tree red-blue-tree?
  (a-red-blue-tree
   (stree red-blue-subtree?)))

(define-datatype red-blue-subtree red-blue-subtree?
  (red-node
   (left red-blue-subtree?)
   (right red-blue-subtree?))
  (blue-node
   (subtrees (list-of red-blue-subtree?)))
  (leaf-node
   (num integer?)))

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

(define mark-leaves-with-red-depth
  (lambda (bt)
    (letrec ((M (lambda (bt depth)
                  (cases red-blue-subtree bt
                         (red-node (left right)
                                   (red-node (M left (+ depth 1))
                                             (M right (+ depth 1))))
                         (blue-node (subtrees)
                                    (blue-node (map (lambda (t) (M t depth))
                                                    subtrees)))
                         (leaf-node (num)
                                    (leaf-node depth))))))
      (cases red-blue-tree bt
             (a-red-blue-tree (stree)
                              (M stree 0))))))

(define tree1 (a-red-blue-tree
               (red-node (red-node (leaf-node 0)
                                   (blue-node (list (leaf-node 0))))
                         (blue-node (list (leaf-node 0)
                                          (leaf-node 0))))))

(equal?? (mark-leaves-with-red-depth tree1)
         (red-node (red-node (leaf-node 2)
                             (blue-node (list (leaf-node 2))))
                   (blue-node (list (leaf-node 1)
                                    (leaf-node 1)))))
