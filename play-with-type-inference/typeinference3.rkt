#lang racket
(require "mk-symbol.rkt")

(define int-rel
  (λ (g exp t)
    (fresh (n)
      (== `(intc ,n) exp)
      (== 'int t))))

(define !-
  (λ (g exp t)
    (conde
      [(int-rel g exp t)])))
