(module environments (lib "eopl.ss" "eopl")

  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm.

  (require "data-structures.rkt")

  (provide init-env empty-env extend-env apply-env extend-env* extend-env-let*)

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

  (define empty-env
    (lambda ()
      (empty-env-record)))

  (define empty-env?
    (lambda (x)
      (empty-env-record? x)))

  (define extend-env
    (lambda (sym val old-env)
      (extended-env-record sym val old-env)))

    ;;; extend-env* : Listof(Var) x Listof(SchemeVal) x Env -> Env
  ;;; see exercise 2-10
  (define extend-env*
    (lambda (varls valls env)
      (cond
        ((and (null? varls) (null? valls))
         env)
        ((and (pair? varls) (pair? valls))
         (let ([var1 (car varls)]
               [val1 (car valls)])
           (extend-env* (cdr varls)
                        (cdr valls)
                        (extend-env var1 val1 env))))
        ((null? varls)
         (report-too-few-variables varls))
        ((null? valls)
         (report-too-few-values valls))
        (else
         (report-wrong-arguments varls valls)))))

  (define extend-env-let*
    (lambda (varls exps eval env)
      (cond
        ((and (null? varls) (null? exps))
         env)
        ((and (pair? varls) (pair? exps))
         (let ([var1 (car varls)]
               [val1 (eval (car exps) env)])
           (extend-env-let* (cdr varls) (cdr exps) eval
                            (extend-env var1 val1 env))))
        ((null? varls)
         (report-too-few-variables varls))
        ((null? exps)
         (report-too-few-values exps))
        (else
         (report-wrong-arguments varls exps)))))


  (define report-too-few-variables
    (lambda (varls)
      (eopl:error 'extend-env
                  "Too few variables ~s" varls)))

  (define report-too-few-values
    (lambda (valls)
      (eopl:error 'extend-env
                  "Too few values ~s." valls)))

  (define report-wrong-arguments
    (lambda (varls valls)
      (eopl:error 'extend-env
                  "Arguments varls: ~s and valls: ~s should be list"
                  varls valls)))

  (define apply-env
    (lambda (env search-sym)
      (if (empty-env? env)
          (eopl:error 'apply-env "No binding for ~s" search-sym)
          (let ((sym (extended-env-record->sym env))
                (val (extended-env-record->val env))
                (old-env (extended-env-record->old-env env)))
            (if (eqv? search-sym sym)
                val
                (apply-env old-env search-sym))))))
  )
