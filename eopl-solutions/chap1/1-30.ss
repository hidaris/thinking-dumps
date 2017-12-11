#lang eopl

(require "base.ss")

;;; sort/predicate : pred x Listof(Int) -> Listof(Int)
;;; usage: (sort/predicate pred loi) returns a list of elements
;;;        sorted by the predicate.
(define sort/predicate
  (lambda (pred loi)
    (letrec
        [(I (lambda (n l)
              (cond
                [(null? l) (cons n '())]
                [else
                 (let [(first (car l))]
                   (if (pred n first)
                       (cons n l)
                       (cons first
                             (I n (cdr l)))))])))]
      (cond
        [(null? loi) '()]
        [else
         (I (car loi)
            (sort/predicate pred (cdr loi)))]))))

(equal?? (sort/predicate < '(8 2 5 2 3))
         '(2 2 3 5 8))

(equal?? (sort/predicate > '(8 2 5 2 3))
         '(8 5 3 2 2))
