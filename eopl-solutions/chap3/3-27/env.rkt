#lang typed/racket

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

(: free-vars (Expression -> (Listof Symbol)))
(define (free-vars exp)
  (match exp
    [(Const n) '()]
    [(Var v) `(,v)]
    [(Op op n1 n2) `(,op)]
    [(IsZero n) (free-vars n)]
    [(Not b) (free-vars b)]
    [(If test then else)
     (set-union (free-vars test)
                (free-vars then)
                (free-vars else))]
    [(Minus n) (free-vars n)]
    [(Let var val body)
     ;; a proc + app
     (set-union (set-remove (free-vars body) var)
                (free-vars val))]
    [(Proc param body)
     (set-subtract (free-vars body)
                   param)]
    [(TraceProc param body)
     (set-subtract (free-vars body)
                   param)]
    [(App proc args)
     (set-union (free-vars proc)
                (cast (flatten (map free-vars args))
                      (Listof Symbol)))]))

(: rebuild-free-env ((Listof Symbol) Environment -> Environment))
(define (rebuild-free-env lst env)
  (cond
    [(null? lst) '()]
    [else (cons `(,(car lst) . ,(apply-env (car lst) env))
                (rebuild-free-env (cdr lst) env))]))

(: init-env
   (-> Environment))
(define (init-env)
  (extend-env
    '* (Closure `(x y) (Op '* (Var 'x) (Var 'y)) (empty-env) #f)
    (extend-env
      '- (Closure `(x y) (Op '- (Var 'x) (Var 'y)) (empty-env) #f)
      (extend-env
        'i (Num 1)
        (extend-env
          'v (Num 5)
          (extend-env
            'x (Num 10)
            (empty-env)))))))
