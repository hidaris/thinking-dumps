#lang eopl

;;; constructors:
;;; var-exp : Var -> Lc-exp
(define var-exp
  (lambda (var)
    var))

;;; lambda-exp : Var x Lc-exp -> Lc-exp
(define lambda-exp
  (lambda (var body)
    (list 'lambda var body)))

;;; app-exp : Lc-exp x Lc-exp -> Lc-exp
(define app-exp
  (lambda (lc-exp1 lc-exp2)
    (list lc-exp1 lc-exp2)))

;;; predicates:
;;; var-exp? : Lc-exp -> Bool
(define var-exp?
  (lambda (exp)
    (symbol? exp)))

;;; lambda-exp? : Lc-exp -> Bool
(define lambda-exp?
  (lambda (exp)
    (and (pair? exp)
         (eqv? (car exp) 'lambda))))

;;; app-exp? : Lc-exp -> Bool
(define app-exp?
  (lambda (exp)
    (and (pair? exp)
         (lambda-exp? (car exp))
         (not (null? (cdr exp))))))

;;; extractors:
;;; var-exp->var : Lc-exp -> Var
(define var-exp->var
  (lambda (exp)
    exp))

;;; lambda-exp->bound-var : Lc-exp -> Var
(define lambda-exp->bound-var
  (lambda (exp)
    (cadr exp)))

;;; lambda-exp->body : Lc-exp -> Lc-exp
(define lambda-exp->body
  (lambda (exp)
    (caddr exp)))

;;; app-exp->rator : Lc-exp -> Lc-exp
(define app-exp->rator
  (lambda (exp)
    (car exp)))

;;; app-exp->rand : Lc-exp -> Lc-exp
(define app-exp->rand
  (lambda (exp)
    (cadr exp)))

;;; occurs-free? : Sym x LcExp -> Bool
(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
      ((lambda-exp? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var exp)))
        (occurs-free? search-var (lambda-exp->body exp))))
      (else
       (or
        (occurs-free? search-var (app-exp->rator exp))
        (occurs-free? search-var (app-exp->rand exp)))))))