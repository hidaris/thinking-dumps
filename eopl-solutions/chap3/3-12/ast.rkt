#lang typed/racket

(provide (all-defined-out))

;;; Expr
(define-type Expr
  (U Const
     Var
     Nullary
     Unary
     Binary
     If
     Let
     Cond))

(struct Const
  ([v : (U Real Boolean)])
  #:transparent)

(struct Unary
  ([op : Var]
   [e : Expr])
  #:transparent)

(struct Binary
  ([op : Var]
   [n1 : Expr]
   [n2 : Expr])
  #:transparent)

(struct If
  ([test : Expr]
   [then : Expr]
   [else : Expr])
  #:transparent)

(struct Var
  ([name : Symbol])
  #:transparent)

(struct Let
  ([var : Var]
   [val : Expr]
   [body : Expr])
  #:transparent)

(struct Cond
  ([lefts  : (Listof Expr)]
   [rights : (Listof Expr)])
  #:transparent)

(struct Nullary
  ([op : Var])
  #:transparent)

;;; Value
(define-type Value
  (U Const
     ConsVal
     EmptyListVal))

(struct ConsVal
  ([e1 : Value]
   [e2 : Value])
  #:transparent)

(struct EmptyListVal
  ()
  #:transparent)

(define-type Program (U AProgram))

(struct AProgram
  ([e : Expr])
  #:transparent)
