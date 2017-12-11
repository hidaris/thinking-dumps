#lang eopl

(require "base.ss")

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
