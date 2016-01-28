#lang racket
(require redex)

(define-language bool-any-lang
  ; boolean
  [B true
     false
     (V B B)]
  ; context
  [C (V C C)
     (V B B)
     hole])

(define B1 (term true))
(define B2 (term false))
(define B3 (term (V true false)))
(define B4 (term (V ,B1 ,B2)))
(define B5 (term (V false ,B4)))
(define C1 (term hole))
(define C2 (term (V (V false false) hole)))
(define C3 (term (V hole true)))

(redex-match bool-any-lang
             B
             (term (V false true)))

(define bool-any-red
  (reduction-relation
   bool-any-lang
   #:domain C
   (--> (in-hole C (V true B))
        (in-hole C true)
        "V-true")
   (--> (in-hole C (V false B))
        (in-hole C B)
        "V-false")))
