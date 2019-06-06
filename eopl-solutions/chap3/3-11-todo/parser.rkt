#lang typed/racket

(require "./ast.rkt")
(require "./utils.rkt")
(provide (all-defined-out))

;; easy version
(: string->sexp (-> String Any))
(define (string->sexp s)
  (read (open-input-string s)))

(: parse-list
   (→ (Listof Expr)
      Expr))
(define (parse-list lst)
  (match lst
    ['() (Nullary (Var 'emptylist))]
    [`(,fst . ,snd)
     (Binary (Var 'cons)
             fst
             (parse-list snd))]))

(: parse-sexp (→ Any Expr))
(define (parse-sexp sexp)
  (match sexp
    [(? real? x) (Const x)]
    [(? symbol? x) (Var x)]
    [`(,(? unary? op) ,n)
     (Unary (Var op) (parse-sexp n))]
    [`(if ,test ,then ,else)
     (If (parse-sexp test)
         (parse-sexp then)
         (parse-sexp else))]
    [`(let ,(? symbol? var) ,val in ,body)
     (Let (Var var)
          (parse-sexp val)
          (parse-sexp body))]
    [`(,(? op? op) ,n1 ,n2)
     (Binary (Var op)
             (parse-sexp n1)
             (parse-sexp n2))]
    [`() (Nullary (Var 'emptylist))]
    [`(list) (Nullary (Var 'emptylist))]
    ;; https://github.com/racket/typed-racket/issues/825 `(list ,e ...)
    [`(list . ,(? list? e))
     (parse-list (map parse-sexp (cdr sexp)))]
    ))

(: parse (→ String Program))
(define (parse str)
  (let ([sexp (string->sexp str)])
    (AProgram (parse-sexp sexp))))
