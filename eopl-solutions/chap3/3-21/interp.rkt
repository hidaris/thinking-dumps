#lang typed/racket

(provide (all-defined-out))

(require "parser.rkt")
(require "ast.rkt")
(require "env.rkt")

(: apply-procedure (Closure Value -> Value))
(define (apply-procedure proc val)
  (match proc
    [(Closure param body saved-env)
     (value-of body (extend-env param val saved-env))]))

(: value-of
   (-> Expression Environment
      Value))
(define (value-of exp env)
  (match exp
    [(Const n) (Num n)]
    [(Var a) (apply-env a env)]
    [(Diff n1 n2)
     (let ([val1 (value-of n1 env)]
           [val2 (value-of n2 env)])
       (let ([sval1 (val->num val1)]
             [sval2 (val->num val2)])
         (Num (- sval1 sval2))))]
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
    [(Proc var body)
     (Closure var body env)]
    [(LetProc name var pbody ebody)
     (let ([closure (Closure var pbody env)])
       (value-of ebody (extend-env name closure env)))]
    [(App proc arg)
     (let ([v1 (value-of proc env)]
           [v2 (value-of arg env)])
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
