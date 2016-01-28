#lang typed/racket

(require "../stack.rkt")

(: suffixes : (∀ (a) ((Stack a) -> (Stack (Stack a)))))
(define (suffixes xs)
  (match xs
    ['() (Cons '() '())]
    [`(,x . ,rest)
     (Cons xs
           (suffixes rest))]))
