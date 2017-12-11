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
    (cons (list varls valls)
          env)))

;;; has-binding? : Env x Var -> Bool
(define has-binding?
  (lambda (env s)
    (letrec
        ((search-in-varlst
          (lambda (henv)
            (if (member? (caar henv))
                #t
                (has-binding? (cdr henv) s))))
         (member?
          (lambda (lst)
            (cond
              ((null? lst) #f)
              ((eq? (car lst) s) #t)
              (else (member? (cdr lst)))))))
      (cond
        ((null? env) #f)
        ((and (pair? env) (pair? (car env)))
         (if (pair? (caar env))
             (search-in-varlst env)
             (let ((saved-var (caar env))
                   (saved-val (cdar env))
                   (saved-env (cdr env)))
               (if (eqv? s saved-var)
                   #t
                   (has-binding? saved-env s)))))
        (else
         (report-invalid-env env))))))

;;; apply-env : Env x Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (letrec
        ((apply-in-varlst
          (lambda (henv)
            (let ((n (get-index (caar henv) 0)))
              (if n
                  (nth-element (cdar henv) n)
                  (apply-env (cdr henv) search-var)))))
         (get-index
          (lambda (lst n)
            (cond
              ((null? lst) #f)
              ((eq? (car lst) search-var) n)
              (else (get-index (cdr lst) (+ n 1))))))
         (nth-element
          (lambda (lst n)
            (cond
              ((null? lst) (report-list-too-short n))
              (else
               (if (zero? n)
                   (car lst)
                   (nth-element (cdr lst) (- n 1)))))))
         (report-list-too-short
          (lambda (n)
            (eopl:error 'nth-element
                        "List too short by ~s elements.~%" (+ n 1)))))
      (cond
        ((null? env) (report-no-binding-found search-var))
        ((and (pair? env) (pair? (car env)))
         (if (pair? (caar env))
             (apply-in-varlst env)
             (let ((saved-var (caar env))
                   (saved-val (cdar env))
                   (saved-env (cdr env)))
               (if (eqv? search-var saved-var)
                   saved-val
                   (apply-env saved-env search-var)))))
        (else
         (report-invalid-env env))))))

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
