#lang typed/racket

(require "./ast.rkt")
(provide parse)

;; easy version
(: string->sexp (→ String Any))
(define (string->sexp s)
  (read (open-input-string s)))

(: parse-bool (-> Any BoolExp))
(define (parse-bool sexp)
  (match sexp
    [`(zero? ,n)
     (IsZero (parse-sexp n))]
    [_ (error 'parse
              "bool exression expected")]))

(: parse-sexp (→ Any Expression))
(define (parse-sexp sexp)
  (match sexp
    [(? real? x)    (Const x)]
    [(? symbol? x)  (Var x)]
    [`(- ,n1 ,n2)
     (Diff (parse-sexp n1)
           (parse-sexp n2))]
    [`(zero? ,x)
     (parse-bool sexp)]
    [`(if ,test ,then ,else)
     (If (parse-bool test)
         (parse-sexp then)
         (parse-sexp else))]
    [`(let ,(? symbol? var) ,val in ,body)
     (Let var
          (parse-sexp val)
          (parse-sexp body))]
    [`(- ,n)
     (Minus (parse-sexp n))]
    ))

(: parse (→ String Program))
(define (parse str)
  (let ([sexp (string->sexp str)])
    (AProgram (parse-sexp sexp))))
