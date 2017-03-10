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
     (cons e e)
     (o e) ;; op for cons: hd, tl
     c)
  (c nil
     number
     cons)
  (o hd tl)
  ; types
  (τ num
      (τ -> τ)
      (τ * τ)
      (case-> (num -> (list num) -> (list num))
              (num -> num -> num))
      Null
      (list τ))
  ; value forms
  (v number
     (λ (x τ) e)
     (cons v v))
  (E hole
     (v E) (E e)
     (if0 E e e)
     (+ E e) (+ v E)
     (cons E e) (cons e E))
  (Γ ·
      (x τ Γ))
  (x variable-not-otherwise-mentioned))

(define-judgment-form stlc
  #:mode (typeof I I O)
  #:contract (typeof Γ e τ)

  [---------------------
   (typeof Γ c (const-type c))]

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

  [(typeof Γ e (list τ))
   -----------------------
   (typeof Γ (o e) τ)]

  [(typeof Γ e (τ_1 * τ))
   ---------------------
   (typeof Γ (hd e) τ_1)]

  [(typeof Γ e (τ * τ_1))
   --------------------
   (typeof Γ (tl e) τ_1)]

  [(typeof Γ e_1 τ)
   (typeof Γ e_2 (list τ))
   ------------------------------------
   (typeof Γ (cons e_1 e_2) (list τ))]

  [(typeof Γ e_1 τ)
   (typeof Γ e_2 Null)
   ------------------------------------
   (typeof Γ (cons e_1 e_2) (list τ))]

  [(typeof Γ e_1 τ_1)
   (typeof Γ e_2 τ_2)
   (side-condition (different Null τ_2)
    )
   ------------------------------------------
   (typeof Γ (cons e_1 e_2) (τ_1 * τ_2))]
  )

(define-metafunction stlc
  const-type : c -> τ
  [(const-type nil) Null]
  [(const-type number) num]
  [(const-type cons) (case-> (num -> (list num) -> (list num))
                             (num -> num -> num))])

(define-metafunction stlc
  different : τ τ -> #t or #f
  [(different τ_1 τ_1) #f]
  [(different τ_1 τ_2) #t])

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
