#lang typed/racket

(require "ast.rkt")
(provide (all-defined-out))

(: val->num
   (-> Value Real))
(define (val->num val)
  (match val
    [(Num n) n]
    [_ (error 'type-mismatch
              "expect type ~s give ~s"
              'Real val)]))

(: val->bool
   (-> Value Boolean))
(define (val->bool val)
  (match val
    [(Bool b) b]
    [_ (error 'type-mismatch
              "expect type ~s give ~s"
              'Boolean val)]))

(: val->closure
   (-> Value Closure))
(define (val->closure val)
  (match val
    [(Closure _ _ _ _) val]
    [_ (error 'type-mismatch
              "expect type ~s give ~s"
              'Closure val)]))

(: val->sval
   (-> Value (U Boolean Real)))
(define (val->sval val)
  (match val
    [(Num n) n]
    [(Bool b) b]
    [_ (error 'type-mismatch
              "expect type ~s give ~s"
              '(U Boolean Real) val)]))

(: value->string
   (-> Value String))
(define (value->string v)
  (match v
    [(Num n) (~v n)]
    [(Bool b) (~v b)]))
