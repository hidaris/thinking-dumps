#lang typed/racket

(provide (all-defined-out))

(require "parser.rkt")
(require "ast.rkt")
(require "env.rkt")

(: apply-procedure (Closure (Listof Value) -> Value))
(define (apply-procedure proc vals)
  (match proc
    [(Closure params body saved-env)
     (define (extend*-env
              [vars : (Listof Symbol)]
              [vals : (Listof Value)]
              [env : Environment]
              ) : Environment
       (cond
         [(null? vars) env]
         [else (extend*-env (cdr vars)
                            (cdr vals)
                            (extend-env
                              (car vars)
                              (car vals)
                              env))]))
     (value-of body (extend*-env params vals saved-env))]))

(: value-of
   (-> Expression Environment
      Value))
(define (value-of exp env)
  (match exp
    [(Const n) (Num n)]
    [(Var a) (apply-env a env)]
    [(Op op n1 n2)
     (let ([val1 (value-of n1 env)]
           [val2 (value-of n2 env)])
       (let ([sval1 (val->num val1)]
             [sval2 (val->num val2)])
         (match op
           ['- (Num (- sval1 sval2))]
           ['* (Num (* sval1 sval2))])))]
    [(IsZero n)
     (let ([val (value-of n env)])
       (let ([sval (val->num val)])
         (if (zero? sval)
             (Bool #t)
             (Bool #f))))]
    [(If test then else)
     (let ([test-val (value-of test env)])
       (if (val->bool test-val)
           (value-of then env)
           (value-of else env)))]
    [(Let var exp body)
     (let ([val (value-of exp env)])
       (value-of body (extend-env var val env)))]
    [(Proc vars body)
     (Closure vars body env)]
    [(LetProc name var pbody ebody)
     (let ([closure (Closure var pbody env)])
       (value-of ebody (extend-env name closure env)))]
    [(App proc args)
     (let ([v1 (value-of proc env)]
           [v2 (map (λ ([x : Expression])
                      (value-of x env))
                    args)])
       (apply-procedure (val->closure v1) v2))]
    [(Minus n)
     (let ([val (value-of n env)])
       (let ([sval (val->num val)])
         (Num (- sval))))]))

(: value-of-program (-> Program Value))
(define (value-of-program pgm)
  (match pgm
    ([AProgram exp1]
     (value-of exp1 (init-env)))))
