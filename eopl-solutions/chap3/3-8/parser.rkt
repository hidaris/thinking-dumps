#lang typed/racket

(require "./ast.rkt")
(provide (all-defined-out))

(: parse (-> Any Expression))
(define (parse sexp)
  (match sexp
    [(? real? x) (Const x)]
    [(? boolean? x) (BoolExp x)]
    [(? symbol? x) (Var x)]
    [`(- ,n1 ,n2)
     (Diff (parse n1) (parse n2))]
    [`(zero? ,n)
     (IsZero (parse n))]
    [`(if ,test ,then ,else)
     (If (parse test) (parse then) (parse else))]
    [`(let ,(? symbol? var) ,val in ,body)
     (Let var (parse val) (parse body))]
    [`(- ,n)
     (Minus (parse n))]
    [`(+ ,n1 ,n2)
     (Add (parse n1) (parse n2))]
    [`(* ,n1 ,n2)
     (Mult (parse n1) (parse n2))]
    [`(/ ,n1 ,n2)
     (Div (parse n1) (parse n2))]
    [`(equal? ,n1 ,n2)
     (IsEqual (parse n1) (parse n2))]
    [`(greater? ,n1 ,n2)
     (IsGreater (parse n1) (parse n2))]
    [`(less? ,n1 ,n2)
     (IsLess (parse n1) (parse n2))]
    ))
