#lang eopl

(require "base.ss")

(define-datatype bintree bintree?
  (leaf-node
   (num integer?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(define leaf-node?
  (lambda (bt)
    (cases bintree bt
           (leaf-node (num)
                      #t)
           (interior-node (key left right)
                          #f))))

(define tree-sum
  (lambda (bt)
    (cases bintree bt
           (leaf-node (num)
                      num)
           (interior-node (key left right)
                          (+ (tree-sum left)
                             (tree-sum right))))))

(define max-interior
  (lambda (bt)
    (cases bintree bt
           (leaf-node (num)
                      (eopl:error 'max-interior
                                  "only accept interior-node"))
           (interior-node (key left right)
                          (if (and (leaf-node? left)
                                   (leaf-node? right))
                              (eopl:error 'max-interior
                                          "bintree should have at least one interior-node")
                              (if (> (tree-sum left) (tree-sum right))
                                  (return-key left)
                                  (return-key right)))))))

(define return-key
  (lambda (bt)
    (cases bintree bt
           (leaf-node (num)
                      #f)
           (interior-node (key left right)
                          (if (and (leaf-node? left)
                                   (leaf-node? right))
                              key
                              (if (leaf-node? left)
                                  (return-key right)
                                  (return-key left)))))))

(define tree-1
  (interior-node 'foo (leaf-node 2) (leaf-node 3)))
(define tree-2
  (interior-node 'bar (leaf-node -1) tree-1))
(define tree-3
  (interior-node 'baz tree-2 (leaf-node 1)))

(equal?? (max-interior tree-2) 'foo)
(equal?? (max-interior tree-3) 'foo)
