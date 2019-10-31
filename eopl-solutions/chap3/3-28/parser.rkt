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
    [(list (? list? x) _ ...) #t]
    [_                #f]))

;; (: list-of-symbol? (Any -> Boolean))
(define (list-of-symbol? x)
  (andmap symbol? x))

(define (list-of-bind? e)
  (and (list? e)
       (eqv? (length e) 2)
       (symbol? (car e))))

;; (: parse-sexp (-> Any Expression))
(define (parse-sexp sexp)
  (match sexp
    [(? real? x) (Const x)]
    [(? symbol? x) (Var x)]
    [`(zero? ,n)
     (IsZero (parse-sexp n))]
    [`(if ,test ,then ,else)
     (If (parse-sexp test)
         (parse-sexp then)
         (parse-sexp else))]
    [`(let ,(? list? e) in ,body)
     #:when (andmap list-of-bind? e)
     (Let (map car e)
          (map parse-sexp (map cadr e))
          (parse-sexp body))]
    ;; [`(let ,(? symbol? var) ,val in ,body)
    ;;  (Let var
    ;;       (parse-sexp val)
    ;;       (parse-sexp body))]
    [`(- ,n)
     (Minus (parse-sexp n))]
    [`(proc ,(? list-of-symbol? var) ,body)
     (Proc var (parse-sexp body))]
    [`(traceproc ,(? list-of-symbol? var) ,body)
     (TraceProc var (parse-sexp body))]
    [`(not ,e)
     (Not (parse-sexp e))]
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
