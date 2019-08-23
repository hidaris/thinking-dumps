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
    [(Let* vars exps body)
     (define (let*-env
              [vars : (Listof Symbol)]
              [exps : (Listof Expression)]
              [env : Environment]) : Environment
       (cond
         [(null? vars) env]
         [else (let*-env (cdr vars)
                         (cdr exps)
                         (extend-env
                           (car vars)
                           (value-of (car exps) env)
                           env))]))
     (if (= (length vars)
            (length exps))
         (value-of body (let*-env vars exps env))
         (error 'value-of "length of let* args should be equal"))]
    [(Unpack vars exps body)
     (let ([vals (map (λ ([exp : Cons]) (value-of exp env)) exps)])
       (if (and (list? vals)
                (= (length vars)
                   (length vals)))
           (let ([kvs (map (λ ([var : (Listof Symbol)] [val : Value])
                             ;; (a b) . (ConsVal (C 2) (Cons (C 3) ..))
                             (cons var val))
                           vars vals)])
             (define (unpack-env
                      [kv  : (Pairof (Listof Symbol) Value)]
                      [env : Environment]) : Environment
               (extend-env (list-ref (car kv) 1)
                           (val->cadr (cdr kv))
                           (extend-env (list-ref (car kv) 0)
                                       (val->car (cdr kv))
                                       env)))
             (value-of body
                       (foldr (λ ([kv : (Pairof (Listof Symbol) Value)]
                                  [env : Environment])
                                (unpack-env kv env))
                              env
                              kvs)))
           (error 'value-of "unpack args number wrong")))]
    [(Minus n)
     (let ([val (value-of n env)])
       (let ([sval (val->num val)])
         (Num (- sval))))]
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
