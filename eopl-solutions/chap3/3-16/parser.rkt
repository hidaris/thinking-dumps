;; #lang typed/racket
#lang racket

(require "ast.rkt")
(provide parse)

;; easy version
;; (: string->sexp (-> String Any))
(define (string->sexp s)
  (read (open-input-string s)))

(define (list-of-bind? e)
  (and (list? e)
       (eqv? (length e) 2)
       (symbol? (car e))))

;; (: parse-bool (-> Any BoolExp))
(define (parse-bool sexp)
  (match sexp
    [`(zero? ,n)
     (IsZero (parse-sexp n))]
    [_ (error 'parse
              "bool exression expected")]))

;; (: parse-sexp (-> Any Expression))
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
    [`(let ,(? list? e) in ,body)
     #:when (andmap list-of-bind? e)
     (Let (map car e)
          (map parse-sexp (map cadr e))
          (parse-sexp body))]
    [`(- ,n)
     (Minus (parse-sexp n))]
    ))

;; (: parse (-> String Program))
(define (parse str)
  (let ([sexp (string->sexp str)])
    (AProgram (parse-sexp sexp))))
