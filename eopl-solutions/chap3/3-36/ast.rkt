#lang typed/racket

(provide (all-defined-out))

;;; Env
(define-type Val (U NormVal RecVal))

(struct NormVal
  ([val : Value])
  #:transparent)

(struct RecVal
  ([vars : (Listof Symbol)]
   [body : Expression])
  #:transparent)

(define-type Bind (Pairof Symbol (U Val (Mutable-Vectorof Any))))

(define-type Environment (Listof Bind))

;;; Expression
(define-type Expression
  (U Const Op Not IsZero If Var Let Minus Proc TraceProc App LetRec))

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
  ([var : (Listof Symbol)]
   [val : (Listof Expression)]
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

(struct TraceProc
  ([param : (Listof Symbol)]
   [body  : Expression])
  #:transparent)

(struct App
  ([proc : Expression]
   [arg  : (Listof Expression)])
  #:transparent)

(struct LetRec
  ([pnames    : (Listof Symbol)]
   [vars-lst  : (Listof (Listof Symbol))]
   [pbodys    : (Listof Expression)]
   [lbody     : Expression])
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

(struct Closure
  ([var  : (Listof Symbol)]
   [body : Expression]
   [env  : Environment]
   [trace : Boolean])
  #:transparent)

(define-type Program (U AProgram))

(struct AProgram
  ([e : Expression])
  #:transparent)