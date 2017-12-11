#lang eopl
;;; A data-structure representation of environments

;;; Env = (empty-env) | (extend-env Var SchemeVal Env)
;;; Var = Sym

;;; empty-env : () -> Env
(define empty-env
  (lambda ()
    (list
     (lambda (search-var)
      (report-no-binding-found search-var))
     (lambda ()
       #t)
     (lambda (s)
       #f))))

;;; extend-env : Var x SchemeVal x Env -> Env
(define extend-env
  (lambda (saved-var saved-val saved-env)
    (list
     (lambda (search-var)
      (if (eqv? search-var saved-var)
          saved-val
          (apply-env saved-env search-var)))
     (lambda ()
       #f)
     (lambda (s)
       (if (eqv? s saved-var)
             #t
             (has-binding? saved-env s))))))

;;; has-binding? : Env x Var -> Bool
(define has-binding?
  (lambda (env s)
    ((caddr env) s)))

;;; apply-env : Env x Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    ((car env) search-var)))

(define empty-env?
  (lambda (env)
    ((cdr env))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env
                "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env
                "Bad environment: ~s" env)))
