#lang typed/racket

(provide (all-defined-out))

(require "parser.rkt")
(require "ast.rkt")
(require "env.rkt")
(require "utils.rkt")

(: value-of-unary (-> Unary Env Value))
(define (value-of-unary unary env)
  (let ([op (Var-name (Unary-op unary))]
        [val (value-of (Unary-e unary) env)])
    (cond
      [(num-unary? op) (num-unary op val)]
      [(list-unary? op) (list-unary op val)]
      [else
       (error
        'value-of "only take num-unary or list-unary but got ~s" op)])))

(: value-of-binary (-> Binary Env Value))
(define (value-of-binary binary env)
  (let ([op (Var-name (Binary-op binary))]
        [val1 (value-of (Binary-n1 binary) env)]
        [val2 (value-of (Binary-n2 binary) env)])
    (cond
      [(num-op? op) (num-binary op val1 val2)]
      [(list-op? op) (list-binary op val1 val2)]
      [else
       (error
        'value-of "only take num-op or list-op but got ~s" op)])))

(: value-of-nullary (-> Nullary Env Value))
(define (value-of-nullary nullary env)
  (let ([op (Nullary-op nullary)])
    (case (Var-name op)
      [(emptylist) (EmptyListVal)]
      [else
       (error 'value-of
              "undefined nullary operator: ~s" op)])))

(: value-of-cond
   (-> (Listof Expr)
       (Listof Expr)
       Env
       Value))
(define value-of-cond
  (lambda (lefts rights env)
    (cond
      [(null? lefts) (error 'cond "No left-hand is true")]
      [(val->bool (value-of (car lefts) env))
       (value-of (car rights) env)]
      [else
       (value-of-cond (cdr lefts) (cdr rights) env)])))


(: value-of (-> Expr Env Value))
(define (value-of exp env)
  (match exp
    [(Const n) (Const n)]
    [(Var a) (apply-env a env)]
    [(Nullary op) (value-of-nullary exp env)]
    [(Unary op n) (value-of-unary exp env)]
    [(Binary op n1 n2) (value-of-binary exp env)]
    [(If test then else)
     (let* ([test-val (value-of test env)]
            [test-sval (val->sval test-val)])
       (define (true-or-not0 val1)
         (cond [(boolean? val1) val1]
               [(real? val1)
                (not (eqv? val1 0))]
               [else #t]))
       (if (true-or-not0 test-sval)
           (value-of then env)
           (value-of else env)))]
    [(Let var exp body)
     (let ([val (value-of exp env)])
       (value-of body (extend-env (Var-name var) val env)))]
    [(Cond test-lst answer-lst)
     (value-of-cond test-lst answer-lst env)]
    ))

(: value-of-program (-> Program Value))
(define (value-of-program pgm)
  (match pgm
    ([AProgram exp1]
     (value-of exp1 (init-env)))))
