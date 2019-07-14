#lang typed/racket

(require "./ast.rkt")
(provide (all-defined-out))

(define-type Env
  (Listof (Pairof Symbol Value)))

(: empty-env
   (→ Env))
(define (empty-env) '())

(: extend-env
   (→ Symbol Value Env Env))
(define (extend-env var val env)
  (cons `(,var . ,val) env))

(: apply-env
   (→ Symbol Env Value))
(define (apply-env var env)
  (cond
    [(assq var env) => cdr]
    [else
     (error 'apply-env
            "var ~s doesn't bound to a value" var)]))

(: init-env
   (→ Env))
(define (init-env)
  (extend-env
    'i (Const 1)
    (extend-env
      'v (Const 5)
      (extend-env
        'x (Const 10)
        (empty-env)))))
