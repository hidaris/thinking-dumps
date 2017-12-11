#lang eopl

;;; A data-structure representation of environments

;;; Env = (empty-env) | (extend-env Var SchemeVal Env)
;;; Var = Sym

(define-datatype env env?
  (empty-env)
  (extend-env
   (svar var?)
   (sval val?)
   (senv env?)))

(define var? symbol?)
(define val?
  (lambda (v)
    #t))

;;; has-binding? : Env x Var -> Bool
(define has-binding?
  (lambda (environment s)
    (cases env environment
           (empty-env () #f)
           (extend-env (svar sval senv)
                       (if (eqv? s svar)
                           #t
                           (has-binding? senv s)))
           (else
            (report-invalid-env environment)))))

;;; apply-env : Env x Var -> SchemeVal
(define apply-env
  (lambda (environment search-var)
    (cases env environment
           (empty-env ()
                      (report-no-binding-found search-var))
           (extend-env (svar sval senv)
                       (if (eqv? search-var svar)
                           sval
                           (apply-env senv search-var)))
           (else
            (report-invalid-env environment)))))

(define report-no-binding-found
  (lambda (search-var)
    (eopl:error 'apply-env
                "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env
                "Bad environment: ~s" env)))
