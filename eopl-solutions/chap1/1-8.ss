#lang eopl

(require "base.ss")

;;; remove-first-without-save : Sym x Listof(Sym) -> Listof(Sym)
;;; usage: (remove-first s los) returns a list with the same
;;;        elements arranged in the same order as los, except that the
;;;        first occurrence of the symbol s is removed, also the symbol
;;;        before first are removed.
(define remove-first-without-save
  (lambda (s los)
    (if (null? los)
        '()
        (if (eqv? (car los) s)
            (cdr los)
            (remove-first-without-save s (cdr los))))))

(equal?? (remove-first-without-save 'a '(b c a d e))
         '(d e))

(equal?? (remove-first-without-save 'a '(a b c d))
         '(b c d))
