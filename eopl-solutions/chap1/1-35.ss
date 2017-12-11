#lang eopl
(require "base.ss")

;;; number-leaves : bintree -> bintree
;;; usage: (number-leaves bt) returns a bintree like the original,
;;;        except the contents of the leaves are numbered starting from
;;;        0.
(define number-leaves
  (lambda (bt)
    (letrec
        [(N (lambda (b n)
              (cond
                [(leaf? b) n]
                [else
                 (interior-node (contents-of b)
                                (N (lson b) (+ n 1))
                                (N (rson b) (+ n 1)))])))]
      (N bt 0))))

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
