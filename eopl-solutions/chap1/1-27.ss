#lang eopl

(require "base.ss")

;;; flatten : slist -> slist
;;; usage: (flatten slist) returns a list of the symbols
;;;        contained in slist in the order in which they occur
;;;        when slist is printed. Intuitively, flatten removes
;;;        all the inner paretheses from its argument.
(define flatten
  (lambda (slist)
    (cond
      ((null? slist) '())
      ((atom? (car slist))
       (cons (car slist)
             (flatten (cdr slist))))
      (else
       (append (flatten (car slist))
               (flatten (cdr slist)))))))

(define atom?
  (lambda (a)
    (and (not (pair? a))
         (not (null? a)))))

(equal?? (flatten '(a b c))
         '(a b c))

(equal?? (flatten '((a) () (b ()) () (c)))
         '(a b c))

(equal?? (flatten '((a b) c (((d)) e)))
         '(a b c d e))

(equal?? (flatten '(a b (() (c))))
         '(a b c))
