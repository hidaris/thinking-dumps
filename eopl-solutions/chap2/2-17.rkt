#lang eopl

;;; use list
;;; constructors:
;;; var-exp : Var -> Lc-exp
(define var-exp
  (lambda (var)
    (list 'var var)))

;;; lambda-exp : Var x Lc-exp -> Lc-exp
(define lambda-exp
  (lambda (var body)
    (list 'lambda (list var) body)))

;;; app-exp : Lc-exp x Lc-exp -> Lc-exp
(define app-exp
  (lambda (lc-exp1 lc-exp2)
    (list 'app lc-exp1 lc-exp2)))

;;; predicates:
;;; var-exp? : Lc-exp -> Bool
(define var-exp?
  (lambda (exp)
    (and (pair? exp)
         (eqv? (car exp) 'var))))

;;; lambda-exp? : Lc-exp -> Bool
(define lambda-exp?
  (lambda (exp)
    (and (pair? exp)
         (eqv? (car exp) 'lambda))))

;;; app-exp? : Lc-exp -> Bool
(define app-exp?
  (lambda (exp)
    (and (pair? exp)
         (eqv? (car exp) 'app))))

;;; extractors:
;;; var-exp->var : Lc-exp -> Var
(define var-exp->var
  (lambda (exp)
    (cadr exp)))

;;; lambda-exp->bound-var : Lc-exp -> Var
(define lambda-exp->bound-var
  (lambda (exp)
    (caadr exp)))

;;; lambda-exp->body : Lc-exp -> Lc-exp
(define lambda-exp->body
  (lambda (exp)
    (caddr exp)))

;;; app-exp->rator : Lc-exp -> Lc-exp
(define app-exp->rator
  (lambda (exp)
    (cadr exp)))

;;; app-exp->rand : Lc-exp -> Lc-exp
(define app-exp->rand
  (lambda (exp)
    (caddr exp)))

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

;;; 2.replace list with vector
;;; constructors:
;;; var-exp : Var -> Lc-exp
(define var-exp2
  (lambda (var)
    (vector 'var var)))

;;; lambda-exp : Var x Lc-exp -> Lc-exp
(define lambda-exp2
  (lambda (var body)
    (vector 'lambda (list var) body)))

;;; app-exp : Lc-exp x Lc-exp -> Lc-exp
(define app-exp2
  (lambda (lc-exp1 lc-exp2)
    (vector 'app lc-exp1 lc-exp2)))

;;; predicates:
;;; var-exp? : Lc-exp -> Bool
(define var-exp2?
  (lambda (exp)
    (and (vector? exp)
         (eqv? (vector-ref exp 0) 'var))))

;;; lambda-exp? : Lc-exp -> Bool
(define lambda-exp2?
  (lambda (exp)
    (and (vector? exp)
         (eqv? (vector-ref exp 0) 'lambda))))

;;; app-exp? : Lc-exp -> Bool
(define app-exp2?
  (lambda (exp)
    (and (vector? exp)
         (eqv? (vector-ref exp 0) 'app))))

;;; extractors:
;;; var-exp->var : Lc-exp -> Var
(define var-exp->var2
  (lambda (exp)
    (vector-ref exp 1)))

;;; lambda-exp->bound-var : Lc-exp -> Var
(define lambda-exp->bound-var2
  (lambda (exp)
    (vector-ref exp 1)))

;;; lambda-exp->body : Lc-exp -> Lc-exp
(define lambda-exp->body2
  (lambda (exp)
    (vector-ref exp 2)))

;;; app-exp->rator : Lc-exp -> Lc-exp
(define app-exp->rator2
  (lambda (exp)
    (vector-ref exp 1)))

;;; app-exp->rand : Lc-exp -> Lc-exp
(define app-exp->rand2
  (lambda (exp)
    (vector-ref exp 2)))

;;; occurs-free? : Sym x LcExp -> Bool
(define occurs-free2?
  (lambda (search-var exp)
    (cond
      ((var-exp2? exp) (eqv? search-var (var-exp->var2 exp)))
      ((lambda-exp2? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var2 exp)))
        (occurs-free2? search-var (lambda-exp->body2 exp))))
      (else
       (or
        (occurs-free2? search-var (app-exp->rator2 exp))
        (occurs-free2? search-var (app-exp->rand2 exp)))))))

;;; 3. use procedural presentation
;;; constructors:
;;; var-exp : Var -> Lc-exp
(define var-exp3
  (lambda (var)
    (list
     (lambda ()
       var)
     (lambda ()
       (eopl:error 'var-exp
                   "not an app-exp"))
     (lambda ()
       #t)
     (lambda ()
       #f)
     (lambda ()
       #f))))

;;; lambda-exp : Var x Lc-exp -> Lc-exp
(define lambda-exp3
  (lambda (var body)
    (list
     (lambda ()
       var)
     (lambda ()
       body)
     (lambda ()
       #f)
     (lambda ()
       #t)
     (lambda ()
       #f))))

;;; app-exp : Lc-exp x Lc-exp -> Lc-exp
(define app-exp3
  (lambda (lc-exp1 lc-exp2)
    (list
     (lambda ()
       lc-exp1)
     (lambda ()
       lc-exp2)
     (lambda ()
       #f)
     (lambda ()
       #f)
     (lambda ()
       #t))))

;;; predicates:
;;; var-exp? : Lc-exp -> Bool
(define var-exp3?
  (lambda (exp)
    ((list-ref exp 2))))

;;; lambda-exp? : Lc-exp -> Bool
(define lambda-exp3?
  (lambda (exp)
    ((list-ref exp 3))))

;;; app-exp? : Lc-exp -> Bool
(define app-exp3?
  (lambda (exp)
    ((list-ref exp 4))))

;;; extractors:
;;; var-exp->var : Lc-exp -> Var
(define var-exp->var3
  (lambda (exp)
    ((list-ref exp 0))))

;;; lambda-exp->bound-var : Lc-exp -> Var
(define lambda-exp->bound-var3
  (lambda (exp)
    ((list-ref exp 0))))

;;; lambda-exp->body : Lc-exp -> Lc-exp
(define lambda-exp->body3
  (lambda (exp)
    ((list-ref exp 1))))

;;; app-exp->rator : Lc-exp -> Lc-exp
(define app-exp->rator3
  (lambda (exp)
    ((list-ref exp 0))))

;;; app-exp->rand : Lc-exp -> Lc-exp
(define app-exp->rand3
  (lambda (exp)
    ((list-ref exp 1))))

;;; occurs-free? : Sym x LcExp -> Bool
(define occurs-free3?
  (lambda (search-var exp)
    (cond
      ((var-exp3? exp) (eqv? search-var (var-exp->var3 exp)))
      ((lambda-exp3? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var3 exp)))
        (occurs-free3? search-var (lambda-exp->body3 exp))))
      (else
       (or
        (occurs-free3? search-var (app-exp->rator3 exp))
        (occurs-free3? search-var (app-exp->rand3 exp)))))))

;;; 4 use struct to define a new datatype.
;;; (struct var-exp (var) #:transparent)
;;; ...