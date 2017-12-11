#lang eopl
(require "base.ss")

;;; double-tree : bintree -> bintree
;;; usage: (double-tree bintree) returns another bintree like the
;;;        original, but with all the integers in the leaves doubled.
(define double-tree
  (lambda (tree)
    (cond
      ((leaf? tree) (leaf (* (contents-of tree) 2)))
      (else
       (interior-node (leaf (contents-of tree))
                      (double-tree (lson tree))
                      (double-tree (rson tree)))))))

(define leaf
  (lambda (i)
    i))

(define interior-node
  (lambda (i l r)
    (list i l r)))

(define leaf?
  (lambda (n)
    (integer? n)))

(define lson
  (lambda (n)
    (cadr n)))

(define rson
  (lambda (n)
    (caddr n)))

(define contents-of
  (lambda (n)
    (if (leaf? n)
        n
        (car n))))

(equal?? (double-tree 2)
         4)

(equal?? (double-tree '(2 (2 1 1) (2 1 1)))
         '(2 (2 2 2) (2 2 2)))
