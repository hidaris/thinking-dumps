#lang eopl

(require "base.ss")

;;; filter-in : pred x List -> List
;;; usage: (filter-in pred lst) returns the list of those elements in
;;;        lst that satisfy the predicate pred.
(define filter-in
  (lambda (pred lst)
    (cond
      ((null? lst) '())
      ((pred (car lst))
       (cons (car lst)
             (filter-in pred
                        (cdr lst))))
      (else
       (filter-in pred
                  (cdr lst))))))

(equal?? (filter-in number? '(a 2 (1 3) b 7))
         '(2 7))

(equal?? (filter-in symbol? '(a (b c) 17 foo))
         '(a foo))
