#lang eopl

(require "base.ss")

;;; duple : Int x SchemeVal -> Listof(SchemeVal)
;;; usage: (duple n x) returns a list containing n copies of x.
(define duple
  (lambda (n x)
    (if (zero? n)
        '()
        (cons x
              (duple (- n 1) x)))))

(equal?? (duple 2 3)
         '(3 3))

(equal?? (duple 4 '(ha ha))
         '((ha ha) (ha ha) (ha ha) (ha ha)))

(equal?? (duple 0 '(blah))
         '())
