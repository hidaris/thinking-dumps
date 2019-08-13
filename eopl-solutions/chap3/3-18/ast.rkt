#lang typed/racket

(provide (all-defined-out))

;;; Expression
(define-type Expression
  (U Const
     BoolExp
     Diff
     If
     Var
     Let
     Let*
     Unpack
     Minus
     Cons
     Car
     Cdr
     IsNull
     EmptyList))

(define-type BoolExp (U IsZero))

(struct Const
  ([n : Real])
  #:transparent)

(struct Diff
  ([n1 : Expression]
   [n2 : Expression])
  #:transparent)

(struct IsZero
  ([n : Expression])
  #:transparent)

(struct If
  ([test : BoolExp]
   [then : Expression]
   [else : Expression])
  #:transparent)

(struct Var
  ([v : Symbol])
  #:transparent)

(struct Let
  ([var : (Listof Symbol)]
   [val : (Listof Expression)]
   [body : Expression])
  #:transparent)

(struct Let*
  ([var : (Listof Symbol)]
   [val : (Listof Expression)]
   [body : Expression])
  #:transparent)

(struct Unpack
  ([var : (Listof (Listof Symbol))]
   [val : (Listof Cons)]
   [body : Expression])
  #:transparent)

;; add minus by 3-6, hidaris
(struct Minus
  ([n : Expression])
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
              "expect type ~s give ~s"
              'Real val)]))

(: val->bool
   (→ Value Boolean))
(define (val->bool val)
  (match val
    [(Bool b) b]
    [_ (error 'type-mismatch
              "expect type ~s give ~s"
              'Boolean val)]))

(: val->car
   (→ Value Value))
(define (val->car val)
  (match val
    [(ConsVal n1 n2) n1]
    [_ (error 'type-mismatch
              "expect type ~s give ~s"
              'ConsVal val)]))

(: val->cdr
   (→ Value Value))
(define (val->cdr val)
  (match val
    [(ConsVal n1 n2) n2]
    [_ (error 'type-mismatch
              "expect type ~s give ~s"
              'ConsVal val)]))

(: val->cadr
   (→ Value Value))
(define (val->cadr val)
  (match val
    [(ConsVal n1 n2)
     (match n2
       [(ConsVal n3 n4) n3]
       [_ (error 'type-mismatch
              "expect type ~s give ~s"
              'ConsVal val)])]
    [_ (error 'type-mismatch
              "expect type ~s give ~s"
              'ConsVal val)]))

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
              "expect type ~s give ~s"
              'Value val)]))

(: value->string
   (→ Value String))
(define (value->string v)
  (match v
    [(Num n) (~v n)]
    [(Bool b) (~v b)]))
