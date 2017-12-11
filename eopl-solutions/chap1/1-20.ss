#lang eopl

(require "base.ss")

;;; count-occurrences : Sym x slist -> Int
;;; usage: (count-occurrences s slist) returns the number of occurrences
;;;        of s in slist.
(define count-occurrences
  (lambda (s slist)
    (cond
      ((null? slist) 0)
      ((atom? (car slist))
       (cond
         ((eq? s (car slist))
          (+ 1 (count-occurrences s (cdr slist))))
         (else (count-occurrences s (cdr slist)))))
      (else
       (+ (count-occurrences s (car slist))
          (count-occurrences s (cdr slist)))))))

(define atom?
  (lambda (a)
    (and (not (null? a))
         (not (pair? a)))))

(equal?? (count-occurrences 'x '((f x) y (((x z) x))))
         3)

(equal?? (count-occurrences 'x '((f x) y (((x z) () x))))
         3)

(equal?? (count-occurrences 'w '((f x) y (((x z) x))))
         0)
