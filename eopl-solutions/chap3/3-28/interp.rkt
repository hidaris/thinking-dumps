#lang typed/racket

(provide (all-defined-out))

(require "parser.rkt")
(require "ast.rkt")
(require "env.rkt")

(: apply-procedure (Closure (Listof Value) Environment -> Value))
(define (apply-procedure proc vals saved-env)
  (match proc
    [(Closure params body trace?)
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
     (let ([result (value-of body
                             (extend*-env params vals saved-env))])
       (when trace? (printf "entry> variables: ~a = ~v\n" params vals))
       (when trace? (printf "exit>  result:    ~v\n" result))
       result)]))

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
    [(Let vars exps body)
     (let ([kvs (map
                 (λ ([var : Symbol]
                     [exp : Expression])
                   (cons var (value-of exp env)))
                 vars
                 exps)])
       (value-of body
                 (foldr
                  (λ ([kv : (Pairof Symbol Value)]
                      [env : Environment])
                    (extend-env (car kv) (cdr kv) env))
                  env
                  kvs)))]
    [(Proc vars body)
     (Closure vars body #f)]
    [(TraceProc vars body)
     (Closure vars body #t)]
    [(App proc args)
     (let ([v1 (value-of proc env)]
           [v2 (map (λ ([x : Expression])
                      (value-of x env))
                    args)])
       (apply-procedure (val->closure v1) v2 env))]
    [(Minus n)
     (let ([val (value-of n env)])
       (let ([sval (val->num val)])
         (Num (- sval))))]
    [(Not b)
     (let ([val (value-of b env)])
       (let ([sval (val->bool val)])
         (Bool (not sval))))]))

(: value-of-program (-> Program Value))
(define (value-of-program pgm)
  (match pgm
    ([AProgram exp1]
     (value-of exp1 (init-env)))))
