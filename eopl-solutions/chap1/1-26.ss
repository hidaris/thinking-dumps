#lang eopl

(require "base.ss")

;;; up : Listof(Listof(SchemeVal)) -> Listof(Listof(SchemeVal))
;;; usage: (up lst) removes a pair of parentheses from each top-level
;;;        element of lst. If a top-level element is not a list, it is
;;;        include in the result, as is. The value of (up (down lst)) is
;;;        equivalent to lst, but (down (up lst)) is not necessarily lst.
(define up
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((list? (car lst))
       (append (car lst)
               (up (cdr lst))))
      (else
       (cons (car lst)
             (up (cdr lst)))))))

(equal?? (up '((1 2) (3 4)))
         '(1 2 3 4))

(equal?? (up '((x (y)) z))
         '(x (y) z))
