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
    ;; const types: +, nil, cons
    [(fresh (_)
       (== '+ e)
       (== '(num -> num -> num) τ))]
    [(fresh (_)
       (== 'nil e)
       (== 'Null τ))]
    [(fresh (_)
       (== 'cons e)
       (== '(case-> (num -> (list num) -> (list num))
                    (num -> num -> (num * num)))
           τ))]
    ;; const types over
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
    ;; cons(num, num)
    [(fresh (e1 e2 τ1 τ2)
       (== `(cons ,e1 ,e2) e)
       (== `(,τ1 * ,τ2) τ)
       (=/= e2 'nil)
       (numbero e2)
       (typeofo Γ e1 τ1)
       (typeofo Γ e2 τ2))]
    ;; cons(num, cons(num, cons(num num)))
    [(fresh (e1 e2 τ1 τ2)
       (== `(cons ,e1 ,e2) e)
       (== `(,τ1 * ,τ2) τ)
       (=/= e2 'nil)
       (fresh (e3 e4)
         (== e2 `(cons ,e3 ,e4))
         (=/= e4 'nil))
       (typeofo Γ e1 τ1)
       (typeofo Γ e2 τ2))]
    ;; cons(1, cons(2, nil)) => '(1 2)
    [(fresh (e1 e2 τ1 τ2)
       (== `(cons ,e1 ,e2) e)
       (== `(list ,τ1) τ)
       (=/= e2 'nil)
       (fresh (e3 e4)
         (== e2 `(cons ,e3 ,e4))
         (== e4 'nil))
       (typeofo Γ e1 τ1)
       (typeofo Γ e2 τ2)
       (== τ1 τ2))]
    ;; cons(1, nil)
    [(fresh (e1 e2 τ1 τ2)
       (== `(cons ,e1 ,e2) e)
       (== `(list ,τ1) τ)
       (== e2 'nil)
       (typeofo Γ e1 τ1)
       (typeofo Γ e2 τ2))]))

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
