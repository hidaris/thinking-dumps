#lang typed/racket/base

(require "ast.rkt")
(provide (all-defined-out))

;; (define-type Environment (Listof (Pairof Symbol Value)))

(: empty-env
   (-> Environment))
(define (empty-env) '())

(: extend-env
   (-> Symbol Value Environment
      Environment))
(define (extend-env var val env)
  (cons `(,var . ,val) env))

(: apply-env
   (-> Symbol Environment
      Value))
(define (apply-env var env)
  (cond
    [(assq var env) => cdr]
    [else
     (error 'apply-env
            "var ~s doesn't bound to a value" var)]))

(: init-env
   (-> Environment))
(define (init-env)
  (extend-env
    '- (Closure `(x y) (Diff (Var 'x) (Var 'y)) (empty-env))
    (extend-env
    'i (Num 1)
    (extend-env
      'v (Num 5)
      (extend-env
        'x (Num 10)
        (empty-env))))))
