#lang racket

(define (f x) (+ x (* x b))) ; forward reference okay here
(define b 3)
(define c (+ b 4)) ; backward reference okay
;; (define d (+ e 4)) ; not okay (get an error instead of #<undefined>)
(define e 5)
[define f 17]
