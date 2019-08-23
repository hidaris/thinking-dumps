#lang typed/racket

(provide (all-defined-out))

(require "parser.rkt")
(require "ast.rkt")
(require "env.rkt")

(: value-of-bool
   (BoolExp Environment -> Bool))
(define (value-of-bool exp env)
  (match exp
    [(IsZero n)
     (let ([val (value-of n env)])
       (let ([sval (val->num val)])
         (if (zero? sval)
             (Bool #t)
             (Bool #f))))]))

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
     (let ([test-val (value-of-bool test env)])
       (if (val->bool test-val)
           (value-of then env)
           (value-of else env)))]
    [(Let vars exps body)
     (let ([kvs (map (λ ([var : Symbol] [exp : Expression])
                       (cons var (value-of exp env)))
                     vars exps)])
       (value-of body
                 (foldr (λ ([kv : (Pairof Symbol Value)] [env : Environment])
                          (extend-env (car kv) (cdr kv) env))
                        env
                        kvs)))]
    [(Minus n)
     (let ([val (value-of n env)])
       (let ([sval (val->num val)])
         (Num (- sval))))]))

(: value-of-program (-> Program Value))
(define (value-of-program pgm)
  (match pgm
    ([AProgram exp1]
     (value-of exp1 (init-env)))))
