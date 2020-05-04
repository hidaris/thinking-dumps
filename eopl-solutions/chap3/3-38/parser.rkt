#lang typed/racket

(require "ast.rkt")
(provide (all-defined-out))

;; easy version
(: string->sexp (-> String Any))
(define (string->sexp s)
  (read (open-input-string s)))

(: def? (Any -> Boolean))
(define (def? x)
  (match x
    [(list 'proc _ _) #t] ;; ((proc _ _) x) or (f x) or ((f x ...) y)
    [(? symbol? x)    #t]
    [(list (? symbol? x) _ ...)    #t]
    [_                #f]))

(: parse-sexp (-> Any Exp))
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
    [`(proc ,(? symbol? var) ,body)
     (Proc var (parse-sexp body))]
    [`(- ,n)
     (Minus (parse-sexp n))]
    [`(,f ,arg)
     (cond
       [(def? f)
        (App (parse-sexp f)
             (parse-sexp arg))]
       [else
        (error 'parse
               "bad app form ~s" sexp)])
     ]
    [`(cond . ,(? list? e))
     #:when (andmap
             (λ (lst) (and (list? lst)
                           (eqv? (length lst) 2))) e)
     (Cond (for/list ((i : Any (in-list e)))
             : (Listof Exp)
             (parse-sexp (car e)))
           (for/list ((i : Any (in-list e)))
             : (Listof Exp)
             (parse-sexp (cadr e))))]
    [_ (error 'parse "unsupport syntax ~s" sexp)]))

(: parse (-> String Program))
(define (parse str)
  (let ([sexp (string->sexp str)])
    (AProgram (parse-sexp sexp))))
