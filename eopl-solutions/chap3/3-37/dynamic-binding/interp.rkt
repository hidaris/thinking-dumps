(module interp (lib "eopl.ss" "eopl")

  ;; interpreter for the PROC language, using the data structure
  ;; representation of procedures.

  ;; The \commentboxes are the latex code for inserting the rules into
  ;; the code in the book. These are too complicated to put here, see
  ;; the text, sorry.

  (require "drscheme-init.rkt")

  (require "lang.rkt")
  (require "data-structures.rkt")
  (require "environments.rkt")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  (define value-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        (add1-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (num-val
               (+ num1 1)))))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
               (- num1 num2)))))

        (mult-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
               (* num1 num2)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                  (bool-val #t)
                  (bool-val #f)))))

        ;\commentbox{\ma{\theifspec}}
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (if (expval->bool val1)
                (value-of exp2 env)
                (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (vars exps body)
          (let ([vals (map (value-in-env env) exps)])
            (value-of body
                      (extend-env* vars vals env))))

        (proc-exp (vars body)
          (proc-val (procedure vars body #f)))

        (traceproc-exp (vars body)
          (proc-val (procedure vars body #t)))

        (letproc-exp (name pvars pbody ebody)
          (let ([proc (proc-val (procedure pvars pbody #f))])
            (value-of ebody
                      (extend-env name proc env))))

        (call-exp (rator rands)
          (let ([proc (expval->proc (value-of rator env))]
                [args (map (lambda (x) (value-of x env)) rands)])
            (apply-procedure proc args env)))

        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc1 vals env)
      (cases proc proc1
        (procedure (vars body trace?)
          (when trace? (eopl:printf "entry> variables: ~a = ~v\n" vars vals))
          (let ([result (value-of body (extend-env* vars vals env))])
            (when trace? (eopl:printf "exit> result: ~v\n" result))
            result)))))

  ;; helper(s)
  (define (value-in-env env)
    (lambda (exp)
      (value-of exp env)))
  )
