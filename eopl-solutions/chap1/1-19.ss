#lang eopl

(require "base.ss")

;;; list-set : Listof(SchemeVal) x Int x SchemeVal -> Listof(SchemeVal)
;;; usage: (list-set lst n x) returns a list like lst, except that the n-th
;;;        element, using zero-based indexing, is x.
(define list-set
  (lambda (lst n x)
    (cond
      ((null? lst) '())
      ((zero? n) (cons x (cdr lst)))
      (else
       (cons (car lst)
             (list-set (cdr lst) (- n 1) x))))))

(equal?? (list-set '(a b c d) 2 '(1 2))
         '(a b (1 2) d))

(equal?? (list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)
         '(1 5 10))
