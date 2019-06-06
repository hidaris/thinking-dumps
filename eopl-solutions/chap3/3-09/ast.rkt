#lang typed/racket

(provide (all-defined-out))

;;; Expression
(define-type Expression
  (U Const
     BoolExp
     Diff
     IsZero
     If
     Var
     Let
     Minus
     Add
     Mult
     Div
     IsGreater
     IsEqual
     IsLess
     Cons
     Car
     Cdr
     IsNull
     EmptyList))

(struct Const
  ([n : Real])
  #:transparent)

(struct BoolExp
  ([n : Boolean])
  #:transparent)

(struct Diff
  ([n1 : Expression]
   [n2 : Expression])
  #:transparent)

(struct IsZero
  ([n : Expression])
  #:transparent)

(struct If
  ([test : Expression]
   [then : Expression]
   [else : Expression])
  #:transparent)

(struct Var
  ([v : Symbol])
  #:transparent)

(struct Let
  ([var : Symbol]
   [val : Expression]
   [body : Expression])
  #:transparent)

;; add minus by 3-6, hidaris
(struct Minus
  ([n : Expression])
  #:transparent)

;; add +, - , * , /, 3-7, by hidaris
(struct Add
  ([n1 : Expression]
   [n2 : Expression])
  #:transparent)

(struct Mult
  ([n1 : Expression]
   [n2 : Expression])
  #:transparent)

(struct Div
  ([n1 : Expression]
   [n2 : Expression])
  #:transparent)

;; add 3-8 equal?, greater?, less? , hidaris
(struct IsEqual
  ([n1 : Expression]
   [n2 : Expression])
  #:transparent)

(struct IsGreater
  ([n1 : Expression]
   [n2 : Expression])
  #:transparent)

(struct IsLess
  ([n1 : Expression]
   [n2 : Expression])
  #:transparent)

;; add 3-9
(struct Cons
  ([e1 : Expression]
   [e2 : Expression])
  #:transparent)

(struct Car
  ([e : Expression])
  #:transparent)

(struct Cdr
  ([e : Expression])
  #:transparent)

(struct IsNull
  ([e : Expression])
  #:transparent)

(struct EmptyList
  ()
  #:transparent)

;;; Value
(define-type Value
  (U Num
     Bool
     ConsVal
     EmptyListVal))

(struct Num
  ([n : Real])
  #:transparent)

(struct Bool
  ([b : Boolean])
  #:transparent)

(struct ConsVal
  ([e1 : Value]
   [e2 : Value])
  #:transparent)

(struct EmptyListVal
  ()
  #:transparent)

(define-type Program (U AProgram))

(struct AProgram
  ([e : Expression])
  #:transparent)

(: val->num
   (→ Value Real))
(define (val->num val)
  (match val
    [(Num n) n]
    [_ (error 'type-mismatch
              "expect type ~s "
              'Real)]))

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

(: val->bool
   (→ Value Boolean))
(define (val->bool val)
  (match val
    [(Bool b) b]
    [_ (error 'type-mismatch
              "expect type ~s"
              'Boolean)]))

(: val->sval
   (→ Value Any))
(define (val->sval val)
  (match val
    [(Num n) n]
    [(Bool b) b]
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
    [(Num n) (~v n)]
    [(Bool b) (~v b)]))
