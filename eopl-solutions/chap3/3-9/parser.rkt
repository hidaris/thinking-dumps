#lang typed/racket

(require "./ast.rkt")
(provide (all-defined-out))

;; easy version
(: string->sexp (-> String Any))
(define (string->sexp s)
  (read (open-input-string s)))

(: parse-sexp (-> Any Expression))
(define (parse-sexp sexp)
  (match sexp
    [(? real? x) (Const x)]
    [(? symbol? x) (Var x)]
    [`(- ,n1 ,n2)
     (Diff (parse-sexp n1)
           (parse-sexp n2))]
    [`(zero? ,n)
     (IsZero (parse-sexp n))]
    [`(if ,test ,then ,else)
     (If (parse-sexp test)
         (parse-sexp then)
         (parse-sexp else))]
    [`(let ,(? symbol? var) ,val in ,body)
     (Let var
          (parse-sexp val)
          (parse-sexp body))]
    [`(- ,n)
     (Minus (parse-sexp n))]
    [`(+ ,n1 ,n2)
     (Add (parse-sexp n1)
          (parse-sexp n2))]
    [`(* ,n1 ,n2)
     (Mult (parse-sexp n1)
           (parse-sexp n2))]
    [`(/ ,n1 ,n2)
     (Div (parse-sexp n1)
          (parse-sexp n2))]
    [`(equal? ,n1 ,n2)
     (IsEqual (parse-sexp n1)
              (parse-sexp n2))]
    [`(greater? ,n1 ,n2)
     (IsGreater (parse-sexp n1)
                (parse-sexp n2))]
    [`(less? ,n1 ,n2)
     (IsLess (parse-sexp n1)
             (parse-sexp n2))]
    [`(cons ,e1 ,e2)
     (Cons (parse-sexp e1)
           (parse-sexp e2))]
    [`(car ,e)
     (Car (parse-sexp e))]
    [`(cdr ,e)
     (Cdr (parse-sexp e))]
    [`(null? ,e)
     (IsNull (parse-sexp e))]
    [`() (EmptyList)]
    ))

(: parse (-> String Program))
(define (parse str)
  (let ([sexp (string->sexp str)])
    (AProgram (parse-sexp sexp))))
