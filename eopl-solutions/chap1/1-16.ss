#lang eopl

(require "base.ss")

;;; invert : Listof(List) -> Listof(List)
;;; usage: (invert lst) where lst is a list of 2-lists
;;;        (lists of length two), return a list with each
;;;        2-list reversed.
(define invert
  (lambda (lst)
    (map reverse lst)))

(equal?? (invert '((a 1) (a 2) (1 b) (2 b)))
         '((1 a) (2 a) (b 1) (b 2)))
