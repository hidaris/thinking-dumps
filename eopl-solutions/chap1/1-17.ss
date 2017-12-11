#lang eopl

(require "base.ss")

;;; down : Listof(SchemeVal) -> Listof(Listof(SchemeVal))
;;; usage: (down lst) wraps parentheses around each top-level
;;;        element of lst.
(define down
  (lambda (lst)
    (letrec
        ((P (lambda (l)
              (cons l '()))))
      (map P lst))))

(equal?? (down '(1 2 3))
         '((1) (2) (3)))

(equal?? (down '((a) (fine) (idea)))
         '(((a)) ((fine)) ((idea))))

(equal?? (down '(a (more (complicated)) object))
         '((a) ((more (complicated))) (object)))
