#lang typed/racket

(provide (all-defined-out))

;;; Exp
(define-type Exp
  (U Const
     Diff
     IsZero
     If
     Var
     Let
     Minus
     Proc
     App
     Cond
     NameLessExp))

(struct Const
  ([n : Real])
  #:transparent)

(struct Diff
  ([n1 : Exp]
   [n2 : Exp])
  #:transparent)

(struct IsZero
  ([n : Exp])
  #:transparent)

(struct If
  ([test : Exp]
   [then : Exp]
   [else : Exp])
  #:transparent)

(struct Var
  ([v : Symbol])
  #:transparent)

(struct Let
  ([var : Symbol]
   [val : Exp]
   [body : Exp])
  #:transparent)

;; add minus by 3-6, hidaris
(struct Minus
  ([n : Exp])
  #:transparent)

(struct Proc
  ([param : Symbol]
   [body  : Exp])
  #:transparent)

(struct App
  ([proc : Exp]
   [args : Exp])
  #:transparent)

(struct Cond
  ([lefts  : (Listof Exp)]
   [rights : (Listof Exp)])
  #:transparent)

(define-type NameLessExp
  (U NameLessVar NameLessLet NameLessProc))

(struct NameLessVar
  ([a : Symbol]
   [n : LexAddr])
  #:transparent)

(struct NameLessLet
  ([val  : Exp]
   [body : Exp])
  #:transparent)

(struct NameLessProc
  ([body : Exp])
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

(define-type NameLessEnv (Listof Value))
(define-type LexAddr Integer)
(define-type Senv (Listof Symbol))

(struct Closure
  ([body : Exp]
   [saved-env : NameLessEnv])
  #:transparent)

(define-type Program (U AProgram))

(struct AProgram
  ([e : Exp])
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

(: val->closure
   (-> Value Closure))
(define (val->closure val)
  (match val
    [(Closure _ _) val]
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
              "expect type ~s"
              '(U Boolean Real))]))

(: value->string
   (-> Value String))
(define (value->string v)
  (match v
    [(Num n) (~v n)]
    [(Bool b) (~v b)]))
