(module environments (lib "eopl.ss" "eopl")

  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm.

  (require "data-structures.scm")

  (provide init-env empty-env extend-env apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69

  (define init-env
    (lambda ()
      (extend-env
       'i (num-val 1)
       (extend-env
        'v (num-val 5)
        (extend-env
         'x (num-val 10)
         (empty-env))))))

  ;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;
  ;;; extend-env* : Listof(Var) x Listof(SchemeVal) x Env -> Env
  ;;; see exercise 2-10
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

  ;; Page: 86
  (define apply-env
    (lambda (env search-sym)
      (cases environment env
        (empty-env ()
          (eopl:error 'apply-env "No binding for ~s" search-sym))
        (extend-env (var val saved-env)
          (if (eqv? search-sym var)
              val
              (apply-env saved-env search-sym)))
        (extend-env-rec (p-name b-var p-body saved-env)
          (if (eqv? search-sym p-name)
            (proc-val (procedure b-var p-body env))
            (apply-env saved-env search-sym))))))

  )
