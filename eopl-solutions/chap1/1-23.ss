#lang eopl

(require "base.ss")

;;; list-index : pred x List -> Int
;;; usage: (list-index pred lst) returns the 0-based position of the
;;;        first element of lst that satisfies the predicate pred.
;;;        If no element of lst satisfies the predicate, then list-index
;;;        returns #f.
(define list-index
  (lambda (pred lst)
    (letrec
        ((L (lambda (lst n)
              (cond
                ((null? lst) #f)
                ((pred (car lst)) n)
                (else
                 (L (cdr lst) (+ n 1)))))))
      (L lst 0))))

(equal?? (list-index number? '(a 2 (1 3) b 7))
         1)

(equal?? (list-index symbol? '(a (b c) 17 foo))
         0)

(equal?? (list-index symbol? '(1 2 (a b) 3))
         #f)
