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
        (let-exp (var exp1 body)
          (let ((val1 (value-of exp1 env)))
            (value-of body
                      (extend-env var val1 env))))

        (proc-exp (vars body)
          (proc-val (procedure vars body env)))

        (letproc-exp (name pvars pbody ebody)
          (let ([proc (proc-val (procedure pvars pbody env))])
            (value-of ebody
                      (extend-env name proc env))))

        (call-exp (rator rands)
          (let ([proc (expval->proc (value-of rator env))]
                [args (map (lambda (x) (value-of x env)) rands)])
            (apply-procedure proc args)))

        )))

  ;; apply-procedure : Proc * ExpVal -> ExpVal
  ;; Page: 79
  (define apply-procedure
    (lambda (proc1 vals)
      (cases proc proc1
        (procedure (vars body saved-env)
          (value-of body (extend-env* vars vals saved-env))))))

  (define free-variables
    (lambda (expr bound)
      (cases expression expr
        (const-exp (num) '())
        (var-exp (var)
          (if (memq var bound)
              '()
              (list var)))
        (diff-exp (exp1 exp2)
          (append (free-variables exp1 bound)
                  (free-variables exp2 bound)))
        (mult-exp (exp1 exp2)
          (append (free-variables exp1 bound)
                  (free-variables exp2 bound)))
        (zero?-exp (arg)
          (free-variables arg bound))
        (if-exp (exp1 exp2 exp3)
          (append (free-variables exp1 bound)
                  (free-variables exp2 bound)))
        (let-exp (var exp1 body)
          (append (free-variables exp1 bound)
                  (free-variables body (cons var bound))))
        (proc-exp (var body)
          (append (free-variables body (cons var bound))))
        (letproc-exp (name pvars pbody ebody)
          (append (free-variables pbody (cons pvars bound))
                  (free-variables ebody (cons name bound))))
        (call-exp (rator rand)
          (append (free-variables rator bound)
                  (free-variables rand bound))))))

  )
