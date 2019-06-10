#lang typed/racket

(provide (all-defined-out))
(require syntax/parse/define)

(define-simple-macro
  (define-datatype (typename:id typevars:id ...)
    (typecons:id [val (~literal :) types] ...) ...)
  (begin (struct (typevars ...) typecons
           ([val : types] ...) #:transparent) ...
        (define-type typename (U typecons ...))))

;;; Expr
(define-datatype (Expr)
  (Const
   [v : (U Real Boolean)])
  (Var
   [name : Symbol])
  (Nullary
   [op : Var])
  (Unary
   [op : Var]
   [e : Expr])
  (Binary
   [op : Var]
   [n1 : Expr]
   [n2 : Expr])
  (If
   [test : Expr]
   [then : Expr]
   [else : Expr])
  (Let
   [var : Var]
   [val : Expr]
   [body : Expr]))

;;; Value
(define-datatype (Value)
  (Const
   )
  (U
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
