#lang eopl

(require "base.ss")

;;; remove : Sym x Listof(Sym) -> Listof(Sym)
;;; usage: (remove s lol) removes all occurrences of a given symbol
;;;        from a list of symbols, not just the first.
(define remove
  (lambda (s los)
    (cond
      ((null? los) '())
      ((eqv? s (car los))
       (remove s (cdr los)))
      (else
       (cons (car los)
             (remove s (cdr los)))))))

(equal?? (remove 'a '(a a b c d))
         '(b c d))

(equal?? (remove 'a '(b d e a d a))
         '(b d e d))
