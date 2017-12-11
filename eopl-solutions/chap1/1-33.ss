#lang eopl

(require "base.ss")

;;; mark-leaves-with-red-depth : bintree -> bintree
;;; usage: (mark-leaves-with-red-depth bt) produces a bintree of the same shape
;;;        as the original, except that in the new tree, each leaf contains
;;;        the integer of nodes between it and the root that contain the symbol
;;;        red.
(define mark-leaves-with-red-depth
  (lambda (bt)
    (letrec
        [(M (lambda (bt depth)
              (cond
                [(leaf? bt) depth]
                [else
                 (let ([new-depth
                        (+ depth
                           (if (eq? (contents-of bt) 'red)
                               1
                               0))])
                   (interior-node (contents-of bt)
                                  (M (lson bt) new-depth)
                                  (M (rson bt) new-depth)))])))]
      (M bt 0))))

(define leaf
  (lambda (i)
    i))

(define interior-node
  (lambda (h l r)
    (list h l r)))

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

(equal?? (mark-leaves-with-red-depth
          (interior-node 'red
                         (interior-node 'bar
                                        (leaf 26)
                                        (leaf 12))
                         (interior-node 'red
                                        (leaf 11)
                                        (interior-node 'quux
                                                       (leaf 117)
                                                       (leaf 14)))))
         '(red (bar 1 1) (red 2 (quux 2 2))))
