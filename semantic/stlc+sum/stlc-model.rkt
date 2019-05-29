#lang racket/base
(require redex racket/match)
(provide stlc typeof)

(define-language stlc
  ; expressions
  (e x
     (λ (x τ) e)
     (e e)
     (if0 e e e)
     (+ e e)
     number
     (e as τ)
     (inl e)
     (inr e)
     (case e of
         (inl x) => e
       ! (inr x) => e))
  ; types
  (τ num
      (τ -> τ)
      (τ + τ))
  ; value forms
  (v number
     (λ (x τ) e)
     (inl v)
     (inr v))
  (E hole
     (v E) (E e)
     (if0 E e e)
     (+ E e) (+ v E)
     (inl E) (inr E)
     (E as τ)
     (case E of
           (inl x) => e
         ! (inr x) => e))
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
   (typeof Γ (λ (x_1 τ_1) e) (τ_1 -> τ))]

  [(typeof Γ e τ)
   -----------------------
   (typeof Γ (e as τ) τ)]

  [(typeof Γ e τ_1)
   ------------------------------------------------
   (typeof Γ ((inl e) as (τ_1 + τ_2)) (τ_1 + τ_2))]

  [(typeof Γ e τ_2)
   ------------------------------------------------
   (typeof Γ ((inr e) as (τ_1 + τ_2)) (τ_1 + τ_2))]

  [(typeof Γ e_1 (τ_1 + τ_2))
   (typeof (x τ_1 Γ) e_2 τ_3)
   (typeof (x τ_2 Γ) e_3 τ_3)
   -----------------------------
   (typeof Γ
           (case e_1 of
                 (inl x) => e_2
               ! (inr x) => e_3)
           τ_3)]
  )

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
