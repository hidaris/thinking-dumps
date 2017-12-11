#lang eopl

(require "base.ss")

;;; product : Listof(Sym) x Listof(Sym) -> Listof(Listof(Sym))
;;; usage: (product sos1 sos2), returns a list of 2-lists that
;;;        represents the Cartesian product of sos1 and sos2.
;;;        The 2-lists may appear in any order.
(define product
  (lambda (sos1 sos2)
    (letrec
        ((P (lambda (s)
              (list (car sos1) s))))
      (cond
        ((null? sos1) '())
        (else
         (append (map P sos2)
                 (product (cdr sos1) sos2)))))))

(equal?? (product '(a b c) '(x y))
         '((a x) (a y) (b x) (b y) (c x) (c y)))
