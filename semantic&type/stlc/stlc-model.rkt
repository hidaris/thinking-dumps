#lang racket/base
(require redex racket/match)
(provide stlc typeof)

(define-language stlc
  (e (e e)
     x
     (λ (x τ) e)
     (if0 e e e)
     (+ e e)
     number)
  (τ (τ -> τ)
      num)
  (v (λ (x τ) e)
     number)
  (E hole
     (v E) (E e)
     (if0 E e e)
     (+ E e) (+ v E))
  (Γ ·
      (x τ Γ))
  (x variable-not-otherwise-mentioned))

(define-judgment-form stlc
  #:mode (typeof I I O)
  #:contract (typeof Γ e τ)

  [---------------------
   (typeof Γ number num)]

  [(where τ (lookup Γ x))
   ----------------------
   (typeof Γ x τ)]

  [(typeof Γ e_1 num)
   (typeof Γ e_2 num)
   --------------------------
   (typeof Γ (+ e_1 e_2) num)]

  [(typeof Γ e_1 num)
   (typeof Γ e_2 τ)
   (typeof Γ e_3 τ)
   ------------------------------
   (typeof Γ (if0 e_1 e_2 e_3) τ)]

  [(typeof Γ e_1 (τ_2 -> τ))
   (typeof Γ e_2 τ_2)
   --------------------------
   (typeof Γ (e_1 e_2) τ)]

  [(typeof (x_1 τ_1 Γ) e τ)
   ----------------------------------------
   (typeof Γ (λ (x_1 τ_1) e) (τ_1 -> τ))])

(define-metafunction stlc
  lookup : Γ x -> τ or #f
  [(lookup (x τ Γ) x) τ]
  [(lookup (x_1 τ Γ) x_2) (lookup Γ x_2)]
  [(lookup · x) #f])

(define (typecheck Γ e)
  (match (judgment-holds (typeof ,Γ ,e τ) τ)
    ['() #f]
    [`(,t) t]
    [_ (error 'typecheck
              "multiple typing derivations for term ~a in environment ~a"
              e Γ)]))
