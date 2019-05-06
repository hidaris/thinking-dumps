#lang typed/racket

(require "./ast.rkt")
(provide (all-defined-out))

(: parse (-> Any Expression))
(define (parse sexp)
  (match sexp
    [(? number? x) (Const x)]
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
    ))
