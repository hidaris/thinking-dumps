#lang typed/racket

(provide TT eq lt)

(define-type TT Integer)

(: eq : (TT TT -> Boolean))
(define (eq a b)
  (= a b))

(: lt : (TT TT -> Boolean))
(define (lt a b)
  (< a b))
