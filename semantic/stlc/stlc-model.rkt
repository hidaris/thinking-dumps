#lang racket/base

(require redex/reduction-semantics racket/match)
(provide stlc typeof)

(define-language stlc
  (e ::=
     x
     z
     (s e)
     (λ (x τ) e)
     (e e))
  (v ::=
     z
     (s v)
     (λ (x τ) e))
  (τ ::=
      nat
      (τ -> τ))
  (E ::=
     hole
     (s E)
     (E e) (v E)
     (+ E e) (+ v E))
  (Γ ::=
      •
      (x τ Γ))
  (γ ::=
     •
     (x v γ))
  (x y ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (λ (x τ) e #:refers-to x))

(define ->val
  (reduction-relation
   stlc
   #:domain e
   (--> (in-hole E (ap (λ (x τ) e) v))
        (in-hole E (substitute e x v))
        β-val)))

(define-judgment-form stlc
  #:mode (typeof I I O)
  #:contract (typeof Γ e τ)

  [---- zero
   (typeof Γ z nat)]

  [(typeof Γ e nat)
   ---- succ
   (typeof Γ (s e) nat)]

  [(where τ (lookup Γ x))
   ---- var
   (typeof Γ x τ)]

  [(typeof (x_1 τ_1 Γ) e τ_2)
   ---- abs
   (typeof Γ (λ (x_1 τ_1) e) (τ_1 -> τ_2))]

  [(typeof Γ e_1 (τ_2 -> τ))
   (typeof Γ e_2 τ_2)
   ---- app
   (typeof Γ (e_1 e_2) τ)]
  )

(define-metafunction stlc
  lookup : Γ x -> τ or #f
  [(lookup (x τ Γ) x) τ]
  [(lookup (x_1 τ Γ) x_2) (lookup Γ x_2)]
  [(lookup · x) #f])

(define-judgment-form stlc
  #:mode (SN I I)
  #:contract (SN τ e)
  [---- not-right
   (SN nat e)])

(define-judgment-form stlc
  #:mode (satisfies I I)
  #:contract (satisfies γ Γ)

  [---- nil
   (satisfies • •)]

  [(SN τ v)
   (satisfies γ Γ)
   ---- cons
   (satisfies (x v γ) (x τ Γ))])

; This is based on Gödel’s T via Harper in Practical Foundations:
(define-extended-language stlc/rec stlc
  [e ::= ....
     (rec e [e_z] [x_pre y_rec e_s])]
  [E ::= ....
     (rec E [e_z] [x_pre y_rec e_s])])

; This is actually call-by-name, because call-by-value requires a dirty hack
; or extra syntax, so far as I can tell.
(define ->val/rec
  (extend-reduction-relation
   ->val stlc/rec
   [--> (in-hole E (rec z [e_z] [x_pre y_rec e_s]))
        (in-hole E e_z)
        rec-zero]
   [--> (in-hole E (rec (s v) [e_z] [x_pre y_rec e_s]))
        (in-hole E (substitute (substitute e_s x_pre v) y_rec (rec v [e_z] [x_pre y_rec e_s])))
        rec-succ]))

;; (define-metafunction stlc
;;   [(lookup (extend Γ y t) x)
;;    (lookup Γ x)
;;    (side-condition (not (equal? (term x) (term y))))])

; This is PCF:
; stlc + fix + if0
(define-extended-language stlc/fix stlc/rec
  [e ::= ....
     (fix e)
     (if0 e e [x e])]
  [E ::= ....
     (fix E)
     (if0 E e [x e])])

(define ->val/fix
  (extend-reduction-relation
   ->val/rec stlc/fix
   [--> (in-hole E (fix (λ (x τ) e)))
        (in-hole E (substitute e x (fix (λ (x τ) e))))
        fix]
   [--> (in-hole E (if0 z e_z [x e_s]))
        (in-hole E e_z)
        if0-z]
   [--> (in-hole E (if0 (s v) e_z [x e_s]))
        (in-hole E (substitute e_s x v))
        if0-s]))

(define-extended-judgment-form stlc/fix typeof
  #:mode (types/alt I I O)
  #:contract (types/alt Γ e τ)

  [(types/alt Γ e nat)
   (types/alt Γ e_z τ)
   (types/alt (y_rec τ (x_pre nat Γ)) e_s τ)
   ---- rec
   (types/alt Γ (rec e [e_z] [x_pre y_rec e_s]) τ)]

  [(types/alt Γ e (-> τ τ))
   ---- fix
   (types/alt Γ (fix e) τ)]

  [(types/alt Γ e nat)
   (types/alt Γ e_z τ)
   (types/alt (x nat Γ) e_s τ)
   ---- if0
   (types/alt Γ (if0 e e_z [x e_s]) τ)])

(define (typecheck Γ e)
  (match (judgment-holds (typeof ,Γ ,e τ) τ)
    ['() #f]
    [`(,t) t]
    [_ (error 'typecheck
              "multiple typing derivations for term ~a in environment ~a"
              e Γ)]))
