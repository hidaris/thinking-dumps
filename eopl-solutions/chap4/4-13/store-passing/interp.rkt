(module interp (lib "eopl.ss" "eopl")

  ;; interpreter for the EXPLICIT-REFS language

  (require "drscheme-init.rkt")

  (require "lang.rkt")
  (require "data-structures.rkt")
  (require "environments.rkt")
  (require "store.rkt")

  (provide value-of-program value-of instrument-let instrument-newref)

;;;;;;;;;;;;;;;; switches for instrument-let ;;;;;;;;;;;;;;;;

  (define instrument-let (make-parameter #f))

  ;; say (instrument-let #t) to turn instrumentation on.
  ;;     (instrument-let #f) to turn it off again.

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 110
  (define value-of-program
    (lambda (pgm)
      ;; (initialize-store!)               ; new for explicit refs.
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env) (empty-store))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 113
  (define value-of
    (lambda (exp env store)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (an-answer (num-val num) store))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (an-answer (apply-env env var) store))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env store)))
            (cases answer val1
              (an-answer (v1 store1)
                (let ((val2 (value-of exp2 env store1)))
                  (cases answer val2
                    (an-answer (v2 store2)
                      (let ((num1 (expval->num v1))
                            (num2 (expval->num v2)))
                        (an-answer (num-val (- num1 num2)) store2)))))))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env store)))
            (cases answer val1
              (an-answer (val store1)
                (let ((num1 (expval->num val)))
                  (an-answer (bool-val (zero? num1)) store1))))))

        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env store)))
            (cases answer val1
              (an-answer (val store1)
                (if (expval->bool val)
                    (value-of exp2 env store1)
                    (value-of exp3 env store1))))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)
          (let ((v1 (value-of exp1 env store)))
            (cases answer v1
              (an-answer (val store1)
                (value-of body
                          (extend-env var val env) store1)))))

        (proc-exp (vars body)
          (an-answer (proc-val (procedure vars body env)) store))

        (call-exp (rator rands)
          (let* ((v1 (value-of rator env store)))
            (cases answer v1
              (an-answer (proc1 store1)
                (letrec
                  ((value-in-env
                    (lambda (env store)
                      (lambda (exp)
                        (answer->value (value-of exp env store))))))
                  (let ((proc (expval->proc proc1))
                        (args (map (value-in-env env store1) rands)))
                    ;; value-of rands shouldn't have effect
                    (apply-procedure proc args store1)))))))

        (letrec-exp (p-names b-vars p-bodies letrec-body)
          (value-of letrec-body
            (extend-env-rec* p-names b-vars p-bodies env) store))

        (begin-exp (exp1 exps)
          (letrec
            ((value-of-begins
               (lambda (e1 es store)
                 (let* ((v1 (value-of e1 env store))
                        (val (answer->value v1))
                        (store1 (answer->store v1)))
                   (if (null? es)
                       (an-answer val store1)
                       (value-of-begins (car es) (cdr es) store1))))))
            (value-of-begins exp1 exps store)))

        (newref-exp (exp1)
          (let ((v1 (value-of exp1 env store)))
            (cases answer v1
              (an-answer (val store1)
                (let ((v2 (newref val store1)))
                  (cases answer v2
                    (an-answer (ref store2)
                      (an-answer (ref-val (expval->num ref)) store2))))))))

        (deref-exp (exp1)
          (let ((v1 (value-of exp1 env store)))
            (cases answer v1
              (an-answer (ref store1)
                (let ((ref1 (expval->ref ref)))
                  (an-answer (deref ref1 store1) store1))))))

        (setref-exp (exp1 exp2)
          (let* ((v1 (value-of exp1 env store))
                 (ref (expval->ref (answer->value v1)))
                 (store1 (answer->store v1))
                 (v2 (value-of exp2 env store1))
                 (val (answer->value v2))
                 (store2 (answer->store v2)))
            (begin
              (let ((store3 (setref! ref val store)))
                (an-answer (num-val 23) store3)))))

        ;; added by 4.11
        (list-exp (list)
          (letrec
            ((value-in-env
              (lambda (env store)
                (lambda (exp)
                  (answer->value (value-of exp env store))))))
            (let ([val (map (value-in-env env store) list)])
              (an-answer (list-val val) store))))
        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;;
  ;; uninstrumented version
  ;;   (define apply-procedure
  ;;    (lambda (proc1 arg)
  ;;      (cases proc proc1
  ;;        (procedure (bvar body saved-env)
  ;;          (value-of body (extend-env bvar arg saved-env))))))

  ;; instrumented version
  (define apply-procedure
    (lambda (proc1 args store)
      (cases proc proc1
        (procedure (vars body saved-env)
          (let ((r args))
            (let ((new-env (extend-env* vars r saved-env)))
              (when (instrument-let)
                (begin
                  (eopl:printf
                   "entering body of proc ~s with env =~%"
                   vars)
                  (pretty-print (env->list new-env))
                  (eopl:printf "store =~%")
                  (pretty-print (store->readable (get-store-as-list store)))
                  (eopl:printf "~%")))
              (value-of body new-env store)))))))


  ;; store->readable : Listof(List(Ref,Expval))
  ;;                    -> Listof(List(Ref,Something-Readable))
  (define store->readable
    (lambda (l)
      (map
        (lambda (p)
          (cons
            (car p)
            (expval->printable (cadr p))))
        l)))

  )
