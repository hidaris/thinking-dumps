#lang typed/racket

(require "ast.rkt")
(provide (all-defined-out))

(: val->car
   (-> Value Value))
(define (val->car val)
  (match val
    [(ConsVal n1 n2) n1]
    [_ (error 'type-mismatch
              "expect type ~s "
              'ConsVal)]))

(: val->cdr
   (-> Value Value))
(define (val->cdr val)
  (match val
    [(ConsVal n1 n2) n2]
    [_ (error 'type-mismatch
              "expect type ~s "
              'ConsVal)]))

(: val->sval
   (-> Value Any))
(define (val->sval val)
  (match val
    [(Const n) n]
    [(EmptyListVal) '()]
    [(ConsVal v1 v2)
     (let ([sval1 (val->sval v1)]
           [sval2 (val->sval v2)])
       (cons sval1 sval2))]
    [_ (error 'type-mismatch
              "expect type ~s"
              'Value)]))

(: value->string
   (-> Value String))
(define (value->string v)
  (match v
    [(Const n) (~v n)]))

(: val->bool
   (-> Value Boolean))
(define (val->bool c)
  (let ([b (val->sval c)])
    (cond
      [(boolean? b) b]
      [else (error 'type-mismatch
                   "only expect type ~s"
                   'Boolean)])))

(: num-binary
   (-> Symbol Value Value Value))
(define (num-binary op n m)
  (let ([n (val->sval n)]
        [m (val->sval m)])
    (cond
      [(and (real? n) (real? m))
       (case op
         [(+) (Const (+ n m))]
         [(-) (Const (- n m))]
         [(*) (Const (* n m))]
         [(/) (Const (/ n m))]
         [(equal?) (Const (equal? n m))]
         [(greater?) (Const (> n m))]
         [(less?) (Const (< n m))]
         [else
          (error 'value-of
                 "undefined binary operator on numbers: ~s" op)])]
      [else (error 'type-mismatch
                   "only expect type ~s"
                   'Real)])))

(: list-binary
   (-> Symbol Value Value Value))
(define (list-binary op n m)
  (case op
    [(cons) (ConsVal n m)]
    [else
     (error 'value-of
            "undefined binary operator on list: ~s" op)]))

(: num-unary
   (-> Symbol Value Value))
(define (num-unary op n)
  (let ([n (val->sval n)])
    (cond
      [(real? n)
       (case op
         [(-) (Const (- n))]
         [(zero?) (Const (zero? n))]
         [else
          (error 'value-of
                 "undefined unary operator on numbers: ~s" op)])]
      [else (error 'type-mismatch
                   "only expect type ~s"
                   'Real)])))

(: list-unary
   (-> Symbol Value Value))
(define (list-unary op val)
  (case op
    [(car) (val->car val)]
    [(cdr) (val->cdr val)]
    [(null?) (match val
               [(EmptyListVal) (Const #t)]
               [_ (Const #f)])]
    [else
     (error 'value-of
            "undefined unary operator on list: ~s" op)]))

(define (op? x)
  (memq x '(+ - * / < <= >= > = eq? cons greater? equal? less?)))

(define (num-op? x)
  (memq x '(+ - * / < <= >= > = eq? greater? equal? less?)))

(define (list-op? x)
  (memq x '(cons)))

(define (unary? x)
  (memq x '(- zero? null? car cdr)))

(define (num-unary? x)
  (memq x '(- zero?)))

(define (list-unary? x)
  (memq x '(null? car cdr)))
