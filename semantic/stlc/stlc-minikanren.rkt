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
       (typeofo Γ e3 τ))]))

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
