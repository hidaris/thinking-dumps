#lang racket

(provide Ͱ run run*)

(require minikanren)
(require "mk-helper.rkt")

(define apply-Γ
  (λ (Γ e τ)
    (symbolo e)
    (fresh (aa da Γ′)
      (== `((,aa . ,da) . ,Γ′) Γ)
      (conde
        [(== aa e) (== da τ)]
        [(=/= aa e) (apply-Γ Γ′ e τ)]))))

(define Ͱ
  (λ (Γ e τ)
    (conde
      [(symbolo e) (apply-Γ Γ e τ)]
      [(== '⊤ τ) (== '⊤ e)]
      [(== '⊥ τ) (== '⊥ e)]
      [(fresh (x b τ₁ τ₂)
         (== `(λ (,x) ,b) e)
         (== `(,τ₁ -> ,τ₂) τ)
         (symbolo x)
         (Ͱ `((,x . ,τ₁) . ,Γ) b τ₂))]
      [(fresh (e₁ e₂ τ₂)
         (== `(,e₁ ,e₂) e)
         (Ͱ Γ e₁ `(,τ₂ -> ,τ))
         (Ͱ Γ e₂ τ₂))]
      [(fresh (a τ₁ τ₂)
         (== `(inl ,a) e)
         (== `(,τ₁ + ,τ₂) τ)
         (Ͱ Γ a τ₁))]
      [(fresh (b τ₁ τ₂)
         (== `(inr ,b) e)
         (== `(,τ₁ + ,τ₂) τ)
         (Ͱ Γ b τ₂))]
      [(fresh (x a l b r τ₁ τ₂)
         (== `(match ,x ((inl ,a) ,l) ((inr ,b) ,r)) e)
         (symbolo a)
         (symbolo b)
         (Ͱ Γ x `(,τ₁ + ,τ₂))
         (Ͱ `((,a . ,τ₁) . ,Γ) l τ)
         (Ͱ `((,b . ,τ₂) . ,Γ) r τ))]
      [(fresh (x τ₁)
         (== `(car ,x) e)
         (Ͱ Γ x `(,τ x ,τ₁)))]
      [(fresh (x τ₁)
         (== `(cdr ,x) e)
         (Ͱ Γ x `(,τ₁ x ,τ)))]
      [(fresh (a b τ₁ τ₂)
         (== `(cons ,a ,b) e)
         (== `(,τ₁ × ,τ₂) τ)
         (Ͱ Γ a τ₁)
         (Ͱ Γ b τ₂))])))
