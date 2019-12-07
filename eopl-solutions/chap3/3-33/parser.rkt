#lang racket

(require "ast.rkt")
;; (provide parse)
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

(define (bind? e)
  (and (eqv? (length e) 2)
       (symbol? (car e))))

(define (rec-def? e)
  (and (list? e)
       (eqv? (length e) 2)
       (list-of-symbol? (car e))
       (def? (cadr e))))

(define (list-of-def? e)
  (andmap rec-def? e))

;; (: parse-sexp (-> Any Expression))
(define (parse-sexp sexp)
  (match sexp
    [(? real? x) (Const x)]
    [(? symbol? x) (Var x)]
    [`(zero? ,n)
     (IsZero (parse-sexp n))]
    [`(- ,n1 ,n2)
     (Op '- (parse-sexp n1)
         (parse-sexp n2))]
    [`(* ,n1 ,n2)
     (Op '* (parse-sexp n1)
         (parse-sexp n2))]
    [`(if ,test ,then ,else)
     (If (parse-sexp test)
         (parse-sexp then)
         (parse-sexp else))]
    [`(let ,(? list? e) in ,body)
     #:when (andmap list-of-bind? e)
     (Let (map car e)
          (map parse-sexp (map cadr e))
          (parse-sexp body))]
    [`(letrec ,(? list-of-def? e) in ,body)
     (LetRec (map caar e)
             (map cdar e)
             (map parse-sexp (map cadr e))
             (parse-sexp body))
     ]
    [`(- ,n)
     (Minus (parse-sexp n))]
    [`(proc ,(? list-of-symbol? vars) ,body)
     (Proc vars (parse-sexp body))]
    [`(traceproc ,(? list-of-symbol? vars) ,body)
     (TraceProc vars (parse-sexp body))]
    [`(not ,e)
     (Not (parse-sexp e))]
    [`(,f ,args ...)
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