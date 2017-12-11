(module interp (lib "eopl.ss" "eopl")

  ;; cps interpreter for the LETREC language, using the data structure
  ;; representation of continuations (Figure 5.3).

  ;; exercise: rewrite this using the procedural representation of
  ;; continuations (Figure 5.2).

  ;; exercise: rewrite this using a trampoline (page 159).

  (require "drscheme-init.scm")

  (require "lang.scm")
  (require "data-structures.scm")
  (require "environments.scm")

  (provide value-of-program value-of/k)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

  ;; value-of-program : Program -> FinalAnswer
  ;; Page: 143 and 154
  (define value-of-program
    (lambda (pgm)
      (cases program pgm
        (a-program (exp1)
          (value-of/k exp1 (init-env) (end-cont))))))

  ;; value-of/k : Exp * Env * Cont -> FinalAnswer
  ;; Page: 143--146, and 154
  (define value-of/k
    (lambda (exp env cont)
      (cases expression exp
        (const-exp (num) (apply-cont cont (num-val num)))
        (var-exp (var) (apply-cont cont (apply-env env var)))
        (proc-exp (var body)
          (apply-cont cont
            (proc-val (procedure var body env))))
        (letrec-exp (p-name b-var p-body letrec-body)
          (value-of/k letrec-body
            (extend-env-rec p-name b-var p-body env)
            cont))
        (zero?-exp (exp1)
           (value-of/k exp1
             env
             (zero1-cont cont)))

        (emptylist-exp () (apply-cont cont (emptylist-val)))

        (null?-exp (exp1)
          (value-of/k exp1 env
            (null?-cont cont)))

        (cons-exp (exp1 exp2)
          (value-of/k exp1
            env
            (cons1-cont exp2 env cont)))

        (car-exp (exp)
          (value-of/k exp env (car-cont cont)))

        (cdr-exp (exp)
          (value-of/k exp env (cdr-cont cont)))

        (list-exp (list)
          (apply-cont (list-cont env cont) list))

        (let-exp (vars exps body)

          (apply-cont (let-exp-cont vars env cont) exps))

        (let2-exp (var1 exp1 var2 exp2 body)
          (value-of/k exp1
            env
            (let2^1-cont var1 var2 exp2 body env cont)))
        (let3-exp (var1 exp1 var2 exp2 var3 exp3 body)
          (value-of/k exp1 env
            (let3^1-cont var1 var2 var3 exp2 exp3 body env cont)))
        (if-exp (exp1 exp2 exp3)
          (value-of/k exp1 env
            (if-test-cont exp2 exp3 env cont)))
        (diff-exp (exp1 exp2)
          (value-of/k exp1 env
            (diff1-cont exp2 env cont)))
        (call-exp (rator rand)
          (value-of/k rator env
            (rator-cont rand env cont)))
   )))

  ;; apply-cont : Cont * ExpVal -> FinalAnswer
  ;; Page: 148
  (define apply-cont
    (lambda (cont val)
      (cases continuation cont
        (end-cont ()
          (begin
            (eopl:printf
              "End of computation.~%")
            val))
        ;; or (logged-print val)  ; if you use drscheme-init-cps.scm
        (zero1-cont (saved-cont)
          (apply-cont saved-cont
            (bool-val
             (zero? (expval->num val)))))
        (null?-cont (saved-cont)
           (apply-cont saved-cont
              (let ([elist (expval->emptylist val)])
                (bool-val (if (null? elist)
                              #t
                              #f)))))
        (car-cont (saved-cont)
          (apply-cont saved-cont
            (expval->car val)))
        (cdr-cont (saved-cont)
          (apply-cont saved-cont
            (expval->cdr val)))
        (cons1-cont (exp2 env cont)
          (value-of/k exp2
            env
            (cons2-cont val cont)))
        (cons2-cont (val1 saved-cont)
           (apply-cont saved-cont
             (cons-val val1 val)))
        (list-cont (env cont)
           (if (null? val)
               (apply-cont cont (emptylist-val))
               (value-of/k (car val)
                 env (list-rest-cont (cdr val) env cont))))
        (list-rest-cont (rest env cont)
           (cons-val val (apply-cont (list-cont env cont) rest)))
        (let-exp-cont (vars body env cont)
           ;; 这里的问题在于是否要将map也cps?
           (let ([vals (map (value-in-env env) val)])
             (value-of/k body
               (extend-env* vars vals env) cont)))
        (let2^1-cont (var1 var2 exp2 body env cont)
           (let ([env2 (extend-env var1 val env)])
             (value-of/k exp2 env
                (let2^2-cont var2 body env2 cont))))
        (let2^2-cont (var2 body env cont)
          (value-of/k body
            (extend-env var2 val env) cont))
        (let3^1-cont (var1 var2 var3 exp2 exp3 body env cont)
           (let ([env2 (extend-env var1 val env)])
             (value-of/k exp2 env
                (let3^2-cont var2 var3 exp3 body env env2 cont))))
        (let3^2-cont (var2 var3 exp3 body env env2 cont)
           (let ([env3 (extend-env var2 val env2)])
             (value-of/k exp3
               env
               (let3^3-cont var3 body env3 cont))))
        (let3^3-cont (var3 body env cont)
           (value-of/k body
             (extend-env var3 val env) cont))
        (if-test-cont (exp2 exp3 saved-env saved-cont)
          (if (expval->bool val)
             (value-of/k exp2 saved-env saved-cont)
             (value-of/k exp3 saved-env saved-cont)))
        (diff1-cont (exp2 saved-env saved-cont)
          (value-of/k exp2
            saved-env (diff2-cont val saved-cont)))
        (diff2-cont (val1 saved-cont)
          (let ((num1 (expval->num val1))
                (num2 (expval->num val)))
            (apply-cont saved-cont
              (num-val (- num1 num2)))))
        ;; 5-8, 和5-7是一种类型，顺便做了
        (rator-cont (rand saved-env saved-cont)
          (let ([args (map (value-in-env saved-env saved-cont) rand)])
            (apply-cont (rand-cont val saved-cont) args)))
        (rand-cont (val1 saved-cont)
          (let ((proc (expval->proc val1)))
            (apply-procedure/k proc val saved-cont)))
        )))

  ;; apply-procedure/k : Proc * ExpVal * Cont -> FinalAnswer
  ;; Page 152 and 155
  (define apply-procedure/k
    (lambda (proc1 args cont)
      (cases proc proc1
        (procedure (vars body saved-env)
          (value-of/k body
            (extend-env* vars args saved-env)
            cont)))))

  ;; helper(s)
  (define (value-in-env env cont)
    (lambda (exp)
      (value-of/k exp env cont)))

  ;; (define list->elist
  ;;   (lambda (list env cont)
  ;;     (cond
  ;;       [(null? list) (emptylist-val)]
  ;;       [else (cons-val (value-of/k (car list) env cont)
  ;;                       (list->elist (cdr list) env cont))])))

  )
