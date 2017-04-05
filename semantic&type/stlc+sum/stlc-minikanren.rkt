#lang racket/base
(require minikanren)

(define (infer expr)
  (let ([res (run* (type)
               (typeofo '() expr type))])
    (if (null? res)
        res
        (car res))))

(define (gen-exp n type)
  (run `,n (expr)
    (typeofo '() expr type)))

(define (typeofo Γ e τ)
  (conde
    [(symbolo e) (lookupo Γ e τ)]
    [(numbero e) (== 'num τ)]
    [(fresh (e1 e2)
       (== `(+ ,e1 ,e2) e)
       (typeofo Γ e1 'num)
       (typeofo Γ e2 'num)
       (== 'num τ))]
    [(fresh (x e^ τ1 τ2)
       (== `(λ (,x ,τ1) ,e^) e)
       (== `(,τ1 -> ,τ2) τ)
       (symbolo x)
       (typeofo `((,x : ,τ1) . ,Γ) e^ τ2))]
    [(fresh (e1 e2 T)
       (== `(,e1 ,e2) e)
       (typeofo Γ e1 `(,T -> ,τ))
       (typeofo Γ e2 T))]
    [(fresh (e1 e2 e3)
       (== `(if0 ,e1 ,e2 ,e3) e)
       (typeofo Γ e1 'num)
       (typeofo Γ e2 τ)
       (typeofo Γ e3 τ))]
    [(fresh (t τ₁ τ₂)
       (== `(inl ,t) e)
       (typeofo Γ t `(,τ₁ + ,τ₂))
       (== τ₁ τ))]
    [(fresh (t τ₁ τ₂)
       (== `(inr ,t) e)
       (typeofo Γ t `(,τ₁ + ,τ₂))
       (== τ₂ τ))]
    [(fresh (e1 e2 e3 x1 x2 τ₁ τ₂ τ₃ τ₄)
       (== `(case ,e1 of
                  (inl ,x1) => ,e2
                ! (inr ,x2) => ,e3) e)
       (typeofo Γ e1 `(,τ₁ + ,τ₂))
       (typeofo `((,x1 : ,τ₁) . ,Γ) e2 τ₃)
       (typeofo `((,x2 : ,τ₂) . ,Γ) e3 τ₃)
       (== τ₃ τ))]))

(define (lookupo Γ x t)
  (fresh ()
    (symbolo x)
    (conde
      [(fresh (_)
         (== `((,x : ,t) . ,_) Γ))]
      [(fresh (y _ Γ^)
         (symbolo y)
         (== `((,y . ,_) . ,Γ^) Γ)
         (=/= x y)
         (lookupo Γ^ x t))])))
