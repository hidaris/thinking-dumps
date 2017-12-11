(module environments (lib "eopl.ss" "eopl")

  (require "data-structures.rkt")
  (provide init-env empty-env extend-env extend-env* apply-env)

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

  (define extend-env*
    (lambda (vars vals env)
      (cond
        ((and (null? vars) (null? vals))
         env)
        ((and (pair? vars) (pair? vals))
         (extend-env* (cdr vars) (cdr vals)
                      (extend-env (car vars) (car vals) env)))
        ((null? vars)
         (report-too-few-variables vars))
        ((null? vals)
         (report-too-few-values vals))
        (else
         (report-wrong-arguments vars vals)))))

  (define report-too-few-variables
    (lambda (vars)
      (eopl:error 'extend-env*
                  "Too few variables ~s" vars)))

  (define report-too-few-values
    (lambda (vals)
      (eopl:error 'extend-env*
                  "Too few values ~s." vals)))

  (define report-wrong-arguments
    (lambda (vars vals)
      (eopl:error 'extend-env*
                  "Arguments vars: ~s and vals: ~s should be list"
                  vars vals)))

  (define apply-env
    (lambda (env search-sym)
      (cases environment env
        (empty-env ()
          (eopl:error 'apply-env "No binding for ~s" search-sym))
        (extend-env (bvar bval saved-env)
          (if (eqv? search-sym bvar)
              bval
              (apply-env saved-env search-sym)))
        (extend-env-rec* (p-names b-vars p-bodies saved-env)
          (cond
            ((location search-sym p-names)
             => (lambda (n)
                  (proc-val
                    (procedure
                      (list-ref b-vars n)
                      (list-ref p-bodies n)
                      env))))
            (else (apply-env saved-env search-sym)))))))

  ;; location : Sym * Listof(Sym) -> Maybe(Int)
  ;; (location sym syms) returns the location of sym in syms or #f is
  ;; sym is not in syms.  We can specify this as follows:
  ;; if (memv sym syms)
  ;;   then (list-ref syms (location sym syms)) = sym
  ;;   else (location sym syms) = #f
  (define location
    (lambda (sym syms)
      (cond
        ((null? syms) #f)
        ((eqv? sym (car syms)) 0)
        ((location sym (cdr syms))
         => (lambda (n)
              (+ n 1)))
        (else #f))))

  )
