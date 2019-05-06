#lang typed/racket

(provide (all-defined-out))

;;; Expression
(define-type Expression
  (U Const
     Diff
     IsZero
     If
     Var
     Let
     Minus))

(struct Const ([n : Number])
  #:transparent)

(struct Diff ([n1 : Expression]
              [n2 : Expression])
  #:transparent)

(struct IsZero ([n : Expression])
  #:transparent)

(struct If ([test : Expression]
            [then : Expression]
            [else : Expression])
  #:transparent)

(struct Var ([v : Symbol])
  #:transparent)

(struct Let ([var : Symbol]
             [val : Expression]
             [body : Expression])
  #:transparent)

;; add minus by 3-6, hidaris
(struct Minus ([val : Expression])
  #:transparent)

;;; Value
(define-type Value
  (U Num
     Bool))

(struct Num ([n : Number])
  #:transparent)

(struct Bool ([b : Boolean])
  #:transparent)

(define-type TopLevel (U ExpTop))
(struct ExpTop ([e : Expression])
  #:transparent)

(define-type Program (U AProgram))
(struct AProgram ([t : TopLevel])
  #:transparent)


(: val->num
   (-> Value Number))
(define (val->num val)
  (match val
    [(Num n) n]
    [_ (error 'type-mismatch
            "~n expect type ~s " 'Number)]))

(: val->bool
   (-> Value Boolean))
(define (val->bool val)
  (match val
    [(Bool b) b]
    [_ (error 'type-mismatch
            "~n expect type ~s" 'Boolean)]))

(: value->string
   (-> Value String))
(define (value->string v)
  (match v
    [(Num n) (~v n)]
    [(Bool b) (~v b)]))
