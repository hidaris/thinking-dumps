#lang racket
(require redex)

(define-language nat
  [N ::= Zero (Plus1 N)])

(define-judgment-form nat
  #:mode (N= I I)
  #:contract (N= N N)
  [
   --- Zero=
   (N= Zero Zero)]
  [
   (where (Plus1 N_0--) N_0)
   (where (Plus1 N_1--) N_1)
   (N= N_0-- N_1--)
   --- Plus1=
   (N= N_0 N_1)])

(define-language Arith
  (e ::= integer (e + e))
  (τ ::= Int))

(define-judgment-form Arith
  #:mode (infer-type I O)
  #:contract (infer-type e τ)
  [
   --- T-Int
   (infer-type e_0 Int)])

;; (define-language SomeTypes
;;   (τ ::= (→ τ τ) Integer))

;; (define-judgment-form SomeTypes
;;   #:mode (<: I I)
;;   #:contract (<: τ τ)
;;   [
;;    (<: τ_0 τ_1)
;;    (<: τ_1 τ_2)
;;    --- S-Trans
;;    (<: τ_0 τ_2)]
;;   [
;;    --- S-Refl
;;    (<: τ_0 τ_0)]
;;   [
;;    (<: τ_dom-1 τ_dom-0)
;;    (<: τ_cod-0 τ_cod-1)
;;    --- S-Arrow
;;    (<: (→ τ_dom-0 τ_cod-0) (→ τ_dom-1 τ_cod-1))])

(define-metafunction nat
  N=? : N N -> boolean
  [(N=? Zero Zero)
   #true]
  [(N=? N_0 N_1)
   (N=? N_0-- N_1--)
   (where (Plus1 N_0--) N_0)
   (where (Plus1 N_1--) N_1)]
  [(N=? N_0 N_1)
   #false])

(define-language Λ
  [e ::= (e e) x (λ x e)]
  [x ::= variable-not-otherwise-mentioned]
  #:binding-forms
  (λ x_0 e_0 #:refers-to x_0))

(alpha-equivalent? Λ
  (term (λ x x))
  (term (λ y y)))
;; #true

(define-metafunction Λ
  test-substitute : e -> e
  [(test-substitute (λ x_0 e_0))
   (substitute e_0 x_0 y)])
(term (test-substitute (λ z (z z))))
;; '(y y)
