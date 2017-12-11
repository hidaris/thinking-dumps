#lang eopl

(require "base.ss")

;;; merge : Listof(Int) x Listof(Int) -> Listof(Int)
;;; usage: (merge loi1 loi2), where loi1 and loi2 are lists of
;;;        integers that are sorted in ascending order, returns
;;;        a sorted list of all the integers in loi1 and loi2.
(define merge
  (lambda (loi1 loi2)
    (cond
      ((null? loi1) loi2)
      ((null? loi2) loi1)
      ((< (car loi1) (car loi2))
       (cons (car loi1)
             (merge (cdr loi1) loi2)))
      (else
       (cons (car loi2)
             (merge loi1 (cdr loi2)))))))

(equal?? (merge '(1 4) '(1 2 8))
         '(1 1 2 4 8))

(equal?? (merge '(35 62 81 90 91) '(3 83 85 90))
         '(3 35 62 81 83 85 90 90 91))
