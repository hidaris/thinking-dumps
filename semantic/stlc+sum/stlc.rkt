#lang racket

;; stil have some problem

(define (empty-env) '())
(define (extend-env var type env)
  (cons `(,var . ,type) env))

(define lookup
  (λ (var Γ)
    (cond
      [(assq var Γ) => cdr]
      [else
       (error 'lookup
              "~n term ~s doesn't bound to a type" var)])))

(define (typeof Γ e)
  (match e
    [(? number?) 'num]
    [`(+ ,n1 ,n2)
     (if (and (eq? (typeof Γ n1) 'num)
              (eq? (typeof Γ n2) 'num))
         'num
         (error '+
                "params type should be num"))]
    [`(if0 ,e1 ,e2 ,e3)
     (let ([τ1 (typeof Γ e1)]
           [τ2 (typeof Γ e2)]
           [τ3 (typeof Γ e3)])
       (if (and (eq? τ1 'num)
                (eq? τ2 τ3))
           τ2
           (error 'if0
                  "if0's branches should have same type")))]
    [(? symbol? x) (lookup x Γ)]
    [`(λ (,x ,τ) ,e^)
     (let ([τ_res (typeof (extend-env x τ Γ) e^)])
       `(,τ -> ,τ_res))]
    [`(,e1 ,e2) ;; ((x -> y) x)
     (let* ([τ (typeof Γ e1)]
            [τ1 (typeof Γ e2)]
            [argt (car τ)])
       (match τ
         [`(,τ1 -> ,τ2) τ2]
         [_ (error 'call
                   "call's params type should be ~s but get ~s"
                   argt τ1)]))]
    [`(,t as ,τ)
     (if (eq? (typeof Γ t) τ)
         τ
         (error 'as
                "type mismatch"))]
    [`((inl ,t) as (,τ₁ + ,τ₂))
     (let ([type (typeof Γ t)])
       (match type
         [`,τ₁ `(,τ₁ + ,τ₂)]
         [_ (error 'inl
                   "not a sum type")]))]
    [`((inr ,t) as (,τ₁ + ,τ₂))
     (let ([type (typeof Γ t)])
       (match type
         [`,τ₂ `(,τ₁ + ,τ₂)]
         [_ (error 'inr
                   "not a sum type")]))]
    [`(case ,e1 of
            (inl ,x) => ,e2
          ! (inr ,x) => ,e3)
     (let ([type (typeof Γ e1)])
       (match type
         [`(,τ₁ + ,τ₂)
          (let ([τ₃ (typeof (extend-env x τ₁ Γ) e2)]
                [τ₄ (typeof (extend-env x τ₂ Γ) e3)])
            (if (eq? τ₃ τ₄)
                τ₃
                (error 'case
                       "inl and inr should be a same type")))]
         [_ (error 'case
                   "not a sum type")]))]
    [_ (error 'typeof
              "unsupport type")]))

(define (typecheck e)
  (typeof (empty-env) e))

;; for debug
(require racket/trace)
(trace typeof)
