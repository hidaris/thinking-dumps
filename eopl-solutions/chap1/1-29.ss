#lang eopl

(require "base.ss")

;;; sort : Listof(Int) -> Listof(Int)
;;; usage: (sort loi) returns a list of the elements of loi in
;;;        ascending order.
(define sort
  (lambda (loi)
    (cond
      [(null? loi) '()]
      [else
       (insert (car loi)
               (sort (cdr loi)))])))

;;; insert : Int x Listof(Int) -> Listof(Int)
;;; to create a list of numbers from n and the numbers
;;; on loi that is sorted in descending order; loi is
;;; already sorted.
(define insert
  (lambda (n loi)
    (cond
      [(null? loi) (cons n '())]
      [else
       (cond
         [(< n (car loi)) (cons n loi)]
         [else
          (cons (car loi)
                (insert n (cdr loi)))])])))

(equal?? (sort '(8 2 5 2 3))
         '(2 2 3 5 8))
