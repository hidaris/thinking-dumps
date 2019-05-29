#lang racket/base

(provide λ-F ->val ∈ ⊢ ⊢/env types)

(require redex/reduction-semantics
         "../util.rkt")

(define-language λ-F
  ;; types
  (t ::=
     a
     (all a t)
     (-> t t))
  ;; expression
  (e ::=
     x
     (λ x t e)
     (ap e e)
     (Λ a e)
     (Ap e t))
  ;; type variable environments
  (Δ ::=
     •
     (extend Δ a))
  ;; typing environments
  (Γ ::=
     •
     (extend Γ x t))
  (v ::=
     (λ x t e)
     (Λ a e))
  (E ::=
     hole
     (ap E e)
     (ap v E)
     (Ap E t))
  ;; variable environments
  (γ ::=
     •
     (extend γ x v))
  ;; variable
  (x y ::= variable-not-otherwise-mentioned)
  ;; type variable
  (a b ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (λ x t e #:refers-to x)
  (Λ a e #:refers-to a)
  (all a t #:refers-to a))

(define ->val
  (reduction-relation
   λ-F
   #:domain e
   (--> (in-hole E (ap (λ x t e) v))
        (in-hole E (substitute e x v))
        β-val)
   (--> (in-hole E (Ap (Λ a e) t))
        (in-hole E (substitute e a t))
        inst)))

(define-metafunction λ-F
  lookup : Γ x -> t
  [(lookup (extend Γ x t) x)
   t]
  [(lookup (extend Γ y t) x)
   (lookup Γ x)
   (side-condition (not (equal? (term x) (term y))))])

(define-judgment-form λ-F
  #:mode (∈ I I)
  #:contract (∈ a Δ)

  [---- found
   (∈ a (extend Δ a))]

  [(∈ a Δ)
   ---- next
   (∈ a (extend Δ b))])

;; whether a type is well formed (which for this language just means closed)
(define-judgment-form λ-F
  #:mode (⊢ I I)
  #:contract (⊢ Δ t)

  [(∈ a Δ)
   ---- var
   (⊢ Δ a)]

  [(⊢ Δ t_1)
   (⊢ Δ t_2)
   ---- arr
   (⊢ Δ (-> t_1 t_2))]

  [(⊢ (extend Δ a) t)
   ---- all
   (⊢ Δ (all a t))])

;; A typing environment is well formed when all the types in it are well formed.
(define-judgment-form λ-F
  #:mode (⊢/env I I)
  #:contract (⊢/env Δ Γ)

  [---- nil
   (⊢/env Δ •)]
  ;; t :: •
  [(⊢ Δ t)
   (⊢/env Δ Γ)
   ---- cons
   (⊢/env Δ (extend Γ x t))])

(define-judgment-form λ-F
  #:mode (types I I I O)
  #:contract (types Δ Γ e t)

  [(⊢/env Δ Γ)
   ---- var
   (types Δ Γ x (lookup Γ x))]

   [(⊢ Δ t_1)
    (types Δ (extend Γ x t_1) e t_2)
   ---- abs
   (types Δ Γ (λ x t_1 e) (-> t_1 t_2))]

  [(types Δ Γ e_1 (-> t_2 t))
   (types Δ Γ e_2 t_2)
   ---- app
   (types Δ Γ (ap e_1 e_2) t)]

  [(types (extend Δ a) Γ e t)
   ---- t-abs
   (types Δ Γ (Λ a e) (all a t))]

  [(⊢ Δ t)
   (types Δ Γ e (all a t_1))
   ---- t-app
   (types Δ Γ (Ap e t) (substitute t_1 a t))])
