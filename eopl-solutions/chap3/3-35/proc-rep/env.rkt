#lang typed/racket

(require "ast.rkt")

(provide (all-defined-out))

(: empty-env
   (-> Environment))
(define (empty-env) '())

(: extend-env
   (Symbol (U Value (Mutable-Vectorof Any)) Environment -> Environment))
(define (extend-env var val env)
  (if (vector? val)
      (cons `(,var . ,val) env)
      (cons `(,var . ,(NormVal val)) env)))

(: extend-env-rec
   (Symbol (Listof Symbol) Expression Environment -> Environment))
(define (extend-env-rec pname bvar-lst pbody env)
  (cons `(,pname . ,(RecVal bvar-lst pbody)) env))

(: proc-item-maker
   (Symbol (Listof Symbol) Expression -> Bind))
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

(: rebuild-env (Environment -> Environment))
(define (rebuild-env env)
  (match env
    [`() `()]
    [`((,sym . ,(? NormVal? val)) . ,saved-env)
     (cons (car env) (rebuild-env saved-env))]
    [`((,pname . ,(? RecVal? val)) . ,saved-env)
     (let ([vec (make-vector 1)])
       (let ([new-env (extend-env pname vec (rebuild-env saved-env))])
         (vector-set! vec 0 (Closure (RecVal-vars val)
                                     (RecVal-body val)
                                     new-env
                                     #f))
         new-env))]))

(: apply-env (Symbol Environment -> Value))
(define (apply-env var env)
  (match env
    [`() (error 'apply-env "No Binding for var ~s" var)]
    [`((,sym . ,(? NormVal? val)) . ,saved-env)
     (if (eqv? var sym)
         (NormVal-val val)
         (apply-env var saved-env))]
    [`((,pname . ,(? vector? val)) . ,saved-env)
     (if (eqv? var pname)
         (cast (vector-ref val 0) Value)
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
