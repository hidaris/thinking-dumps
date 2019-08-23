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
     Minus))

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
  ([var : Symbol]
   [val : Expression]
   [body : Expression])
  #:transparent)

;; add minus by 3-6, hidaris
(struct Minus
  ([n : Expression])
  #:transparent)

;;; Value
(define-type Value
  (U Num
     Bool))

(struct Num
  ([n : Real])
  #:transparent)

(struct Bool
  ([b : Boolean])
  #:transparent)

(define-type Program (U AProgram))

(struct AProgram
  ([e : Expression])
  #:transparent)


(: val->num
   (-> Value Real))
(define (val->num val)
  (match val
    [(Num n) n]
    [_ (error 'type-mismatch
              "expect type ~s "
              'Real)]))

(: val->bool
   (-> Value Boolean))
(define (val->bool val)
  (match val
    [(Bool b) b]
    [_ (error 'type-mismatch
              "expect type ~s"
              'Boolean)]))

(: val->sval
   (-> Value (U Boolean Real)))
(define (val->sval val)
  (match val
    [(Num n) n]
    [(Bool b) b]
    [_ (error 'type-mismatch
              "expect type ~s"
              '(U Boolean Real))]))

(: value->string
   (-> Value String))
(define (value->string v)
  (match v
    [(Num n) (~v n)]
    [(Bool b) (~v b)]))
