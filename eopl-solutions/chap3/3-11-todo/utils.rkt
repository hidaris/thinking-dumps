#lang typed/racket

(require "./ast.rkt")
(provide (all-defined-out))

(: val->car
   (→ Value Value))
(define (val->car val)
  (match val
    [(ConsVal n1 n2) n1]
    [_ (error 'type-mismatch
              "expect type ~s "
              'ConsVal)]))

(: val->cdr
   (→ Value Value))
(define (val->cdr val)
  (match val
    [(ConsVal n1 n2) n2]
    [_ (error 'type-mismatch
              "expect type ~s "
              'ConsVal)]))

(: val->sval
   (→ Value Any))
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
   (→ Value String))
(define (value->string v)
  (match v
    [(Const n) (~v n)]))

(: val->bool
   (→ Value Boolean))
(define (val->bool c)
  (cond
    [(and (Const? c)
          (boolean? (Const-v c)))
     (Const-v c)]
    [else (error 'val->bool
                 "only expect type ~s"
                 'Boolean)]))

(: arith-binary
   (→ Symbol Const Const Value))
(define (arith-binary op n m)
  (let ([n (Const-v n)]
        [m (Const-v m)])
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
            (abort 'arith-binary
                   "undefined operator on numbers: " op)])]
      [else (error 'arith-binary
                   "only expect type ~s"
                   'Real)])))

(: arith-unary
   (→ Symbol Const Value))
(define (arith-unary op n)
  (let ([n (Const-v n)])
    (cond
      [(real? n)
       (case op
         [() (Const (- n))]
         [(zero?) (Const (zero? n))]
         [else
            (abort 'arith-unary
                   "undefined operator on numbers: " op)])]
      [else (error 'arith-unary
                   "only expect type ~s"
                   'Real)])))

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

(define (abort who . args)
  (printf "~s: " who)
  (for-each display args)
  (display "\n")
  (error 'infer ""))
