(module interp (lib "eopl.ss" "eopl")

  ;; interpreter for the LET language.  The \commentboxes are the
  ;; latex code for inserting the rules into the code in the book.
  ;; These are too complicated to put here, see the text, sorry.

  (require "drscheme-init.rkt")

  (require "lang.rkt")
  (require "data-structures.rkt")
  (require "environments.rkt")

  (provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> ExpVal
  ;; Page: 71
  (define value-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
                   (value-of exp1 (init-env))))))

  ;; value-of : Exp * Env -> ExpVal
  ;; Page: 71
  (define value-of
    (lambda (exp env)
      (cases expression exp

        ;\commentbox{ (value-of (const-exp \n{}) \r) = \n{}}
        (const-exp (num) (num-val num))

        (minus-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num (expval->num val1)))
              (num-val (- num)))))

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

        (plus-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
               (+ num1 num2)))))
        (mult-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
               (* num1 num2)))))
        (div-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
               (/ num1 num2)))))

        (null?-exp (exp1)
          (let ((val (value-of exp1 env)))
            (let ((lst (expval->empty-list val)))
              (if (null? lst)
                  (bool-val #t)
                  (bool-val #f)))))

        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                  (bool-val #t)
                  (bool-val #f)))))

        (greater?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (if (> num1 num2)
                  (bool-val #t)
                  (bool-val #f)))))
        (equal?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (if (eqv? num1 num2)
                  (bool-val #t)
                  (bool-val #f)))))
        (less?-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (if (< num1 num2)
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

        (empty-list-exp ()
                        (empty-list-val))
        (cons-exp (car cdr)
          (let ((val1 (value-of car env))
                (val2 (value-of cdr env)))
            (cons-val val1 val2)))
        (car-exp (body)
          (let ((val1 (value-of body env)))
            (expval->car val1)))
        (cdr-exp (body)
          (let ((val1 (value-of body env)))
            (expval->cdr val1)))
        (list-exp (body)
          (let ((val1 (map (value-of-curry env) body)))
            (list-val val1)))

        )))

  ; helper: apply for every elem in list.
  (define value-of-curry
    (lambda (env)
      (lambda (body)
        (value-of body env))))

  )
