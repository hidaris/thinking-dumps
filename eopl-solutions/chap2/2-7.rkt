#lang eopl
;;; A data-structure representation of environments

;;; Env = (empty-env) | (extend-env Var SchemeVal Env)
;;; Var = Sym

;;; empty-env : () -> Env
(define empty-env
  (lambda ()'()))

;;; extend-env : Var x SchemeVal x Env -> Env
(define extend-env
  (lambda (var val env)
    (cons `(,var ,val) env)))

;;; apply-env : Env x Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (cond
      ((null? env)
       (report-no-binding-found search-var env))
      ((and (pair? env) (pair? (car env)))
       (let ((saved-var (caar env))
             (saved-val (cdar env))
             (saved-env (cdr env)))
         (if (eqv? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
      (else
       (report-invalid-env env)))))

(define report-no-binding-found
  (lambda (search-var env)
    (eopl:error 'apply-env
                "No binding for ~s in env ~s" search-var env)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env
                "Bad environment: ~s" env)))