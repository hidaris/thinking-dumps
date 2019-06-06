#lang typed/racket

(require "./ast.rkt")
(provide (all-defined-out))

(define-type Env
  (Listof (Pairof Var Value)))

(: empty-env
   (→ Env))
(define (empty-env) '())

(: extend-env
   (→ Var Value Env Env))
(define (extend-env var val env)
  (cons `(,var . ,val) env))

(: apply-env
   (→ Var Env Value))
(define (apply-env var env)
  (cond
    [(assq (Var-name var) env) => cdr]
    [else
     (error 'apply-env
            "var ~s doesn't bound to a value" (Var-name var))]))

(: init-env
   (→ Env))
(define (init-env)
  (extend-env
    (Var 'i) (Const 1)
    (extend-env
      (Var 'v) (Const 5)
      (extend-env
        (Var 'x) (Const 10)
        (empty-env)))))
