#lang racket

(require "ast.rkt")
(provide (all-defined-out))

;; easy version
;; (: string->sexp (-> String Any))
(define (string->sexp s)
  (read (open-input-string s)))

;; (: def? (Any -> Boolean))
(define (def? x)
  (match x
    [(list 'proc _ _) #t] ;; ((proc _ _) x) or (f x) or ((f x ...) y)
    [(? symbol? x)    #t]
    [(list (? symbol? x) _ ...)    #t]
    [_                #f]))

;; (: list-of-symbol? (Any -> Boolean))
(define (list-of-symbol? x)
  (andmap symbol? x))

;; (: parse-sexp (-> Any Expression))
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
    [`(letproc ,(? symbol? name)
               ,(? list-of-symbol? var) = ,pbody
               in ,ebody)
     (LetProc name
              var
              (parse-sexp pbody)
              (parse-sexp ebody))]
    [`(- ,n)
     (Minus (parse-sexp n))]
    [`(proc ,(? list-of-symbol? var) ,body)
     (Proc var (parse-sexp body))]
    [`(,f ,arg ...)
     (cond
       [(def? f)
        (App (parse-sexp f)
             (map parse-sexp (cdr sexp)))]
       [else
        (error 'parse
               "bad app form ~s" sexp)])
     ]))

;; (: parse (-> String Program))
(define (parse str)
  (let ([sexp (string->sexp str)])
    (AProgram (parse-sexp sexp))))
