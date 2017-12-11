#lang eopl

(require "base.ss")

;;; exists? : pred x List -> Boolean
;;; usage: (exists? pred lst) returns #t if any element of lst satisfies
;;;        pred, and returns #f otherwise.
(define exists?
  (lambda (pred lst)
    (cond
      ((null? lst) #f)
      ((pred (car lst)) #t)
      (else
       (exists? pred (cdr lst))))))

(equal?? (exists? number? '(a b c 3 e))
         #t)

(equal?? (exists? number? '(a b c d e))
         #f)
