#lang racket
(require redex)
(require "common.rkt")

(define-extended-language Lambda-calculus Lambda
  (e ::= .... n)
  (n ::= natural)
  (v ::= (lambda (x ...) e))
  ; a context is an expression with one hole in lieu of a subexpression
  (C ::=
     hole
     (e ... C e ...)
     (lambda (x_!_ ...) C)))

(define Context? (redex-match? Lambda-calculus C))

(module+ test
  (define C1 (term ((lambda (x y) x) hole 1)))
  (define C2 (term ((lambda (x y) hole) 0 1)))
  (test-equal (Context? C1) #true)
  (test-equal (Context? C2) #true))

;; use in-hole to fill the hole of context, and
;; yields an expression
;; such as in-hole (λ x. hole 2) 3 => λ x.3 2
;; what is a hole, it the current caculus expression,
;; which with a continuation.

(define -->β
  (reduction-relation
   Lambda-calculus
   (--> (in-hole C ((lambda (x_1 ..._n) e) e_1 ..._n))
        (in-hole C (subst ([e_1 x_1] ...) e)))))

(define -->βv
  (reduction-relation
   Lambda-calculus
   (--> (in-hole C ((lambda (x_1 ..._n) e) v_1 ..._n))
        (in-hole C (subst ([v_1 x_1] ...) e)))))
