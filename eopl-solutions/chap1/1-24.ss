#lang eopl

(require "base.ss")

;;; every? : pred x List -> Boolean
;;; usage: (every? pred lst) returns #f if any element of lst fails
;;;        to satisfy pred, and returns #t otherwise.
(define every?
  (lambda (pred lst)
    (cond
      ((null? lst) #t)
      ((pred (car lst))
       (every? pred (cdr lst)))
      (else #f))))

(equal?? (every? number? '(a b c 3 e))
         #f)

(equal?? (every? number? '(1 2 3 5 4))
         #t)
