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

        ;\commentbox{ (value-of (var-exp \x{}) \r) = (apply-env \r \x{})}
        (var-exp (var) (apply-env env var))

        ;; add by 3-6
        (minus-exp (exp)
          (let ([val (value-of exp env)])
            (let ([num (expval->num val)])
              (num-val (- num)))))

        ;\commentbox{\diffspec}
        (diff-exp (exp1 exp2)
          (let ((val1 (value-of exp1 env))
                (val2 (value-of exp2 env)))
            (let ((num1 (expval->num val1))
                  (num2 (expval->num val2)))
              (num-val
               (- num1 num2)))))

        ;; add by 3-7
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


        ;\commentbox{\zerotestspec}
        (zero?-exp (exp1)
          (let ((val1 (value-of exp1 env)))
            (let ((num1 (expval->num val1)))
              (if (zero? num1)
                  (bool-val #t)
                  (bool-val #f)))))

        ;; add by 3-8
        (equal?-exp (exp1 exp2)
          (let ([val1 (value-of exp1 env)]
                [val2 (value-of exp2 env)])
            (let ([num1 (expval->num val1)]
                  [num2 (expval->num val2)])
              (bool-val (if (= num1 num2)
                            #t
                            #f)))))

        (greater?-exp (exp1 exp2)
          (let ([val1 (value-of exp1 env)]
                [val2 (value-of exp2 env)])
            (let ([num1 (expval->num val1)]
                  [num2 (expval->num val2)])
              (bool-val (if (> num1 num2)
                            #t
                            #f)))))

        (less?-exp (exp1 exp2)
          (let ([val1 (value-of exp1 env)]
                [val2 (value-of exp2 env)])
            (let ([num1 (expval->num val1)]
                  [num2 (expval->num val2)])
              (bool-val (if (< num1 num2)
                            #t
                            #f)))))

        ;; add 3-9
        (emptylist-exp () (emptylist-val))

        (cons-exp (exp1 exp2)
          (let ([car (value-of exp1 env)]
                [cdr (value-of exp2 env)])
            (cons-val car cdr)))

        (car-exp (exp)
          (let ([val (value-of exp env)])
            (expval->car val)))

        (cdr-exp (exp)
          (let ([val (value-of exp env)])
            (expval->cdr val)))

        (null?-exp (exp)
          (let ([val (value-of exp env)])
            (let ([elist (expval->emptylist val)])
              (bool-val (if (null? elist)
                            #t
                            #f)))))

        ;\commentbox{\ma{\theifspec}}
        ;; changed by 3-13,hidaris, make 0 as false, others true
        (if-exp (exp1 exp2 exp3)
          (let ((val1 (value-of exp1 env)))
            (define (true-or-not0 val1)
              (cond [(bool? val1)
                     (expval->bool val1)]
                    [(num? val1)
                     (not (eqv? (expval->num val1) 0))]
                    [else #t]))
            (if (true-or-not0 val1)
                (value-of exp2 env)
                (value-of exp3 env))))

        ;\commentbox{\ma{\theletspecsplit}}
        (let-exp (var exp1 body)
          (let ((val1 (value-of exp1 env)))
            (value-of body
                      (extend-env var val1 env))))

        ;; add by 3-10, hidaris
        (list-exp (list)
          (let ([val (map (value-in-env env) list)])
            (slist->dlist val)))

        ;; add by 3-12, hidaris
        (cond-exp (lefts rights)
          (cond-val lefts rights env))
        )))

  ;; helper(s)
  (define (value-in-env env)
    (lambda (exp)
      (value-of exp env)))

  (define cond-val
    (lambda (lefts rights env)
      (cond
        [(null? lefts)
         (eopl:error 'cond-val
                     "No left-hand is true")]
        [(expval->bool (value-of (car lefts) env))
         (value-of (car rights) env)]
        [else
         (cond-val (cdr lefts) (cdr rights) env)])))

  ;; add by 3-13, hidaris
  ;; predicates help judgment
  (define num?
    (lambda (val)
      (cases expval val
        (num-val (v) #t)
        (else #f))))

  (define bool?
    (lambda (val)
      (cases expval val
        (bool-val (v) #t)
        (else #f))))
  )