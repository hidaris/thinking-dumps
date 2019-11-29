#lang typed/racket

(require "ast.rkt")

(provide (all-defined-out))

(: empty-env
   (-> Environment))
(define (empty-env) '())

(: extend-env
   (Symbol Value Environment -> Environment))
(define (extend-env var val env)
  (cons `(,var . ,(NormVal val)) env))

(: extend-env-rec
   (Symbol (Listof Symbol) Expression Environment -> Environment))
(define (extend-env-rec pname bvar-lst pbody env)
  (cons `(,pname . ,(RecVal bvar-lst pbody)) env))

(: proc-item-maker
   (Symbol (Listof Symbol) Expression -> (Pairof Symbol Val)))
(define proc-item-maker
  (λ (pname bvars body)
    (cons pname (RecVal bvars body))))

(: extend-env-rec*
   ((Listof Symbol)
    (Listof (Listof Symbol))
    (Listof Expression)
    Environment
    ->
    Environment))
(define (extend-env-rec* pnames bvar-lsts pbodys env)
  (append
   (map proc-item-maker pnames bvar-lsts pbodys) env))


(: apply-env (Symbol Environment -> Value))
(define (apply-env var env)
  (match env
    [`()
     (error 'apply-env "No Binding for var ~s" var)]
    [`((,sym . ,(? NormVal? val)) . ,saved-env)
     (if (eqv? var sym)
         (NormVal-val val)
         (apply-env var saved-env))]
    [`((,pname . ,(? RecVal? val)) . ,saved-env)
     (if (eqv? var pname)
         (Closure (RecVal-vars val)
                  (RecVal-body val)
                  env
                  #f)
         (apply-env var saved-env))]))

(: init-env
   (-> Environment))
(define (init-env)
  (extend-env
    'i (Num 1)
    (extend-env
      'v (Num 5)
      (extend-env
        'x (Num 10)
        (empty-env)))))
