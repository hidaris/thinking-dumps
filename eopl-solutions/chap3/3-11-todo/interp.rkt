#lang typed/racket

(provide (all-defined-out))

(require "./parser.rkt")
(require "./ast.rkt")
(require "./env.rkt")
(require "./utils.rkt")

(: value-of-unary (→ Unary Env Value))
(define (value-of-unary unary env)
  (let ([op (Var-name (Unary-op unary))]
        [val (value-of (Unary-e unary) env)])
    (cond
      [(num-unary? op) (arith-unary op val)]
      [(list-unary? op)
       (case op
         [(car) (val->car val)]
         [(cdr) (val->cdr val)]
         [(null?) (match val
                    [(EmptyListVal) (Const #t)]
                    [_ (Const #f)])]
         [else
          (abort 'value-of
                 "undefined operator on numbers: " op)])]
      [else
       (abort 'value-of
              "only operate on number or list, but got: " (Unary-e unary))])))

(: value-of-binary (→ Binary Env Value))
(define (value-of-binary binary env)
  (let ([op (Var-name (Binary-op binary))]
        [val1 (value-of (Binary-n1 binary) env)]
        [val2 (value-of (Binary-n2 binary) env)])
    (cond
      [(num-op? op) (arith-binary op val1 val2)]
      [(list-op? op)
       (case op
         [(cons) (ConsVal val1 val2)]
         [else
          (abort 'value-of "undefined operator on numbers: " op)])]
      [else
       (abort
        'value-of "only operate on numbers or , but got: " (Binary-n1 binary) "," (Binary-n2 binary))])))

(: value-of-nullary (→ Nullary Env Value))
(define (value-of-nullary nullary env)
  (let ([op (Nullary-op nullary)])
    (case (Var-name op)
      [(emptylist) (EmptyListVal)]
      [else
       (abort 'value-of
              "undefined operator on numbers: " op)])))

(: value-of (→ Expr Env Value))
(define (value-of exp env)
  (match exp
    [(Const n) (Const n)]
    [(Var a) (apply-env exp env)]
    [(Nullary op) (value-of-nullary exp env)]
    [(Unary op n) (value-of-unary exp env)]
    [(Binary op n1 n2) (value-of-binary exp env)]
    [(If test then else)
     (let ([test-val (value-of test env)])
       (if (val->bool test-val)
           (value-of then env)
           (value-of else env)))]
    [(Let var exp body)
     (let ([val (value-of exp env)])
       (value-of body (extend-env var val env)))]
    ))

(: value-of-program (→ Program Value))
(define (value-of-program pgm)
  (match pgm
    ([AProgram exp1]
     (value-of exp1 (init-env)))))
