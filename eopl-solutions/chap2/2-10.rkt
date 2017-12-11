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

;;; extend-env* : Listof(Var) x Listof(SchemeVal) x Env -> Env
(define extend-env*
  (lambda (varls valls env)
    (cond
      ((and (null? varls) (null? valls))
       env)
      ((and (pair? varls) (pair? valls))
       (extend-env* (cdr varls)
                    (cdr valls)
                    (extend-env (car varls) (car valls) env)))
      ((null? varls)
       (report-too-few-variables varls))
      ((null? valls)
       (report-too-few-values valls))
      (else
       (report-wrong-arguments varls valls)))))

;;; has-binding? : Env x Var -> Bool
(define has-binding?
  (lambda (env s)
    (cond
      ((null? env) #f)
      ((and (pair? env) (pair? (car env)))
       (let ((saved-var (caar env))
             (saved-val (cdar env))
             (saved-env (cdr env)))
         (if (eqv? s saved-var)
             #t
             (has-binding? saved-env s))))
      (else
       (report-invalid-env env)))))

;;; apply-env : Env x Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (cond
      ((null? env)
       (report-no-binding-found search-var))
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
  (lambda (search-var)
    (eopl:error 'apply-env
                "No binding for ~s" search-var)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env
                "Bad environment: ~s" env)))

(define report-too-few-variables
  (lambda (varls)
    (eopl:error 'extend-env*
                "Too few variables ~s" varls)))

(define report-too-few-values
  (lambda (valls)
    (eopl:error 'extend-env*
                "Too few values ~s." valls)))

(define report-wrong-arguments
  (lambda (varls valls)
    (eopl:error 'extend-env*
                "Arguments varls: ~s and valls: ~s should be list"
                varls valls)))
