#lang typed/racket

(require "ast.rkt")

(provide translation-of-program)
;;;;;;;;;;;;;;;; lexical address calculator ;;;;;;;;;;;;;;;;

;; translation-of-program : Program -> Nameless-program
;; Page: 96
(: translation-of-program (-> Program Program))
(define (translation-of-program pgm)
  (match pgm
    [(AProgram exp1)
     (AProgram
      (translation-of exp1 (init-senv)))]))

;; translation-of : Exp * Senv -> Nameless-exp
;; Page 97
(: translation-of (-> Exp Senv [#:skip Boolean] Exp))
(define (translation-of exp senv #:skip [skip #f])
  (match exp
    [(Const num) exp]
    [(Diff exp1 exp2)
     (Diff
      (translation-of exp1 senv)
      (translation-of exp2 senv))]
    [(IsZero exp1)
     (IsZero
      (translation-of exp1 senv))]
    [(If exp1 exp2 exp3)
     (If
      (translation-of exp1 senv)
      (translation-of exp2 senv #:skip #t)
      (translation-of exp3 senv #:skip #t))]
    [(Var var)
     (NameLessVar
      var
      (apply-senv senv var #:skip skip))]
    [(Let var exp1 body)
     (NameLessLet
      (translation-of exp1 senv)
      (translation-of body (extend-senv var senv)))]
    [(Proc var body)
     (NameLessProc
      (translation-of body (extend-senv var senv)))]
    [(App rator rand)
     (App
      (translation-of rator senv)
      (translation-of rand senv))]
    [(Minus e)
     (Minus
      (translation-of e senv))]
    [(Cond lefts rights)
     (Cond
      (map (λ ([x : Exp]) (translation-of x senv)) lefts)
      (map (λ ([x : Exp]) (translation-of x senv)) rights))]
    [_ (report-invalid-source-expression exp)]))

(define report-invalid-source-expression
  (lambda (exp)
    (error 'value-of
           "Illegal expression in source code: ~s" exp)))

;;;;;;;;;;;;;;;; static environments ;;;;;;;;;;;;;;;;

;; Page: 95
(: empty-senv (-> Senv))
(define (empty-senv) '())

;; Page: 95
(: extend-senv (-> Symbol Senv Senv))
(define (extend-senv var senv)
  (cons var senv))

;; Page: 95
(: apply-senv (-> Senv Symbol [#:skip Boolean] LexAddr))
(define (apply-senv senv var #:skip [skip #f])
  (cond
    [(equal? skip #t) -1]
    [(null? senv) (report-unbound-var var)]
    [(eqv? var (car senv)) 0]
    [else
     (+ 1 (apply-senv (cdr senv) var))]))

(define report-unbound-var
  (lambda (var)
    (error 'translation-of "unbound variable in code: ~s" var)))

;; Page: 96
(: init-senv (-> Senv))
(define (init-senv)
  (extend-senv
   'i
   (extend-senv
    'v
    (extend-senv
     'x
     (empty-senv)))))
