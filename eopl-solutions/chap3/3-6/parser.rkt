#lang typed/racket

(require "./ast.rkt")
(provide (all-defined-out))

;; easy version
(: string->sexp (→ String Any))
(define (string->sexp s)
  (read (open-input-string s)))

(: parse-sexp (→ Any Expression))
(define (parse-sexp sexp)
  (match sexp
    [(? real? x) (Const x)]
    [(? symbol? x) (Var x)]
    [`(- ,n1 ,n2)
     (Diff (parse-sexp n1) (parse-sexp n2))]
    [`(zero? ,n)
     (IsZero (parse-sexp n))]
    [`(if ,test ,then ,else)
     (If (parse-sexp test) (parse-sexp then) (parse-sexp else))]
    [`(let ,(? symbol? var) ,val in ,body)
     (Let var (parse-sexp val) (parse-sexp body))]
    [`(- ,n)
     (Minus (parse-sexp n))]
    ))

(: parse (→ String Program))
(define (parse str)
  (let ([sexp (string->sexp str)])
    (AProgram (parse-sexp sexp))))
