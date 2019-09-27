#lang typed/racket

(provide (all-defined-out))

;;; Expression
(define-type Expression
  (U Const Op Not IsZero If Var Let Minus Proc App LetProc))

(struct Const
  ([n : Real])
  #:transparent)

(struct Op
  ([op : Symbol]
   [n1 : Expression]
   [n2 : Expression])
  #:transparent)

(struct IsZero
  ([n : Expression])
  #:transparent)

(struct Not
  ([b : Expression])
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
  ([var  : Symbol]
   [val  : Expression]
   [body : Expression])
  #:transparent)

;; add minus by 3-6, hidaris
(struct Minus
  ([n : Expression])
  #:transparent)

(struct Proc
  ([param : (Listof Symbol)]
   [body  : Expression])
  #:transparent)

(struct LetProc
  ([name  : Symbol]
   [param : (Listof Symbol)]
   [pbody : Expression]
   [ebody : Expression])
  #:transparent)

(struct App
  ([proc : Expression]
   [args : (Listof Expression)])
  #:transparent)

;;; Value
(define-type Value
  (U Num
     Bool
     Closure))

(struct Num
  ([n : Real])
  #:transparent)

(struct Bool
  ([b : Boolean])
  #:transparent)

(define-type Environment (Listof (Pairof Symbol Value)))

(struct Closure
  ([var  : (Listof Symbol)]
   [body : Expression]
   [env  : Environment])
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
    [(Closure _ _ _) val]
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
