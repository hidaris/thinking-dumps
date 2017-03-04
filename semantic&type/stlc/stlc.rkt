#lang racket

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
    [(? symbol? x) (lookup Γ x)]
    [`(λ (,x ,τ) ,e^)
     `(,τ -> ,(typeof (extend-env x τ Γ) e^))]
    [`(,e1 ,e2) ;; ((x -> y) x)
     (let* ([τ1 (typeof Γ e1)]
            [τ2 (typeof Γ e2)]
            [argτ (car τ1)])
       (if (eq? argτ τ2)
           (list-ref τ1 2)
           (error 'call
                  "call's params type should be ~s but get ~s"
                  argτ τ2)))]
    [_ (error 'typeof
              "unsupport type")]))
