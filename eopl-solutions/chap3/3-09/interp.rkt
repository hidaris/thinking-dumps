#lang typed/racket

(provide (all-defined-out))

(require "parser.rkt")
(require "ast.rkt")
(require "env.rkt")

(: value-of
   (-> Expression Environment
       Value))
(define (value-of exp env)
  (match exp
    [(Const n) (Num n)]
    [(BoolExp b) (Bool b)]
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
    [(Minus n)
     (let ([val (value-of n env)])
       (let ([sval (val->num val)])
         (Num (- sval))))]
    [(Add n1 n2)
     (let ([val1 (value-of n1 env)]
           [val2 (value-of n2 env)])
       (let ([sval1 (val->num val1)]
             [sval2 (val->num val2)])
         (Num (+ sval1 sval2))))]
    [(Mult n1 n2)
     (let ([val1 (value-of n1 env)]
           [val2 (value-of n2 env)])
       (let ([sval1 (val->num val1)]
             [sval2 (val->num val2)])
         (Num (* sval1 sval2))))]
    [(Div n1 n2)
     (let ([val1 (value-of n1 env)]
           [val2 (value-of n2 env)])
       (let ([sval1 (val->num val1)]
             [sval2 (val->num val2)])
         (Num (/ sval1 sval2))))]
    [(IsEqual n1 n2)
     (let ([val1 (value-of n1 env)]
           [val2 (value-of n2 env)])
       (let ([sval1 (val->num val1)]
             [sval2 (val->num val2)])
         (if (equal? sval1 sval2)
             (Bool #t)
             (Bool #f))))]
    [(IsGreater n1 n2)
     (let ([val1 (value-of n1 env)]
           [val2 (value-of n2 env)])
       (let ([sval1 (val->num val1)]
             [sval2 (val->num val2)])
         (Bool (if (> sval1 sval2)
                   #t
                   #f))))]
    [(IsLess n1 n2)
     (let ([val1 (value-of n1 env)]
           [val2 (value-of n2 env)])
       (let ([sval1 (val->num val1)]
             [sval2 (val->num val2)])
         (Bool (if (< sval1 sval2)
                   #t
                   #f))))]
    [(EmptyList) (EmptyListVal)]
    [(Cons e1 e2)
     (let ([val1 (value-of e1 env)]
           [val2 (value-of e2 env)])
       (ConsVal val1 val2))]
    [(Car e)
     (let ([val (value-of e env)])
       (val->car val))]
    [(Cdr e)
     (let ([val (value-of e env)])
       (val->cdr val))]
    [(IsNull e)
     (let ([val (value-of e env)])
       (match val
         [(EmptyListVal) (Bool #t)]
         [_ (Bool #f)]))]))

(: value-of-program (-> Program Value))
(define (value-of-program pgm)
  (match pgm
    ([AProgram exp1]
     (value-of exp1 (init-env)))))
