(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.scm")                  ; for expression?

  (provide (all-defined-out))               ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
      (value number?))
    (bool-val
      (boolean boolean?))
    (proc-val
      (proc proc?))
    (cons-val
     (car expval?)
     (cdr expval?))
    (emptylist-val))

;;; extractors:

  (define expval->num
    (lambda (v)
      (cases expval v
        (num-val (num) num)
        (else (expval-extractor-error 'num v)))))

  (define expval->bool
    (lambda (v)
      (cases expval v
        (bool-val (bool) bool)
        (else (expval-extractor-error 'bool v)))))

  (define expval->proc
    (lambda (v)
      (cases expval v
        (proc-val (proc) proc)
        (else (expval-extractor-error 'proc v)))))

  (define expval->car
    (lambda (v)
      (cases expval v
        (cons-val (car cdr) car)
        (else (expval-extractor-error 'car v)))))

  (define expval->cdr
    (lambda (v)
      (cases expval v
        (cons-val (car cdr) cdr)
        (else (expval-extractor-error 'cdr v)))))

  (define expval->emptylist
    (lambda (v)
      (cases expval v
        (emptylist-val () '())
        (else (expval-extractor-error 'emptylist v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                  variant value)))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

  ;; Page: 148
  (define identifier? symbol?)

  (define-datatype continuation continuation?
    (end-cont)
    (zero1-cont
     (saved-cont continuation?))
    (null?-cont
     (saved-cont continuation?))
    (car-cont
     (saved-cont continuation?))
    (cdr-cont
     (saved-cont continuation?))
    (cons1-cont
     (exp2 expression?)
     (saved-env environment?)
     (saved-cont continuation?))
    (cons2-cont
     (val1 expval?)
     (saved-cont continuation?))
    (list-cont
     (saved-env environment?)
     (saved-cont continuation?))
    (list-rest-cont
     (rest (list-of expression?))
     (saved-env environment?)
     (saved-cont continuation?))
    (let-exp-cont
      (vars (list-of identifier?))
      (body expression?)
      (saved-env environment?)
      (saved-cont continuation?))
    (if-test-cont
      (exp2 expression?)
      (exp3 expression?)
      (saved-env environment?)
      (saved-cont continuation?))
    (diff1-cont
      (exp2 expression?)
      (saved-env environment?)
      (saved-cont continuation?))
    (diff2-cont
      (val1 expval?)
      (saved-cont continuation?))
    (rator-cont
      (rand expression?)
      (saved-env environment?)
      (saved-cont continuation?))
    (rand-cont
      (val1 expval?)
      (saved-cont continuation?))
    (let2^1-cont
     (var1 identifier?)
     (var2 identifier?)
     (exp2 expression?)
     (body expression?)
     (saved-env environment?)
     (saved-cont continuation?))
    (let2^2-cont
     (var2 identifier?)
     (body expression?)
     ;; 这里的env是扩展了let2^1的var1之后的环境，也就是说数据环境可以在控制环境中增长
     (saved-env environment?)
     (saved-cont continuation?))
    (let3^1-cont
     (var1 identifier?)
     (var2 identifier?)
     (var3 identifier?)
     (exp2 expression?)
     (exp3 expression?)
     (body expression?)
     (saved-env environment?)
     (saved-cont continuation?))
    (let3^2-cont
     (var2 identifier?)
     (var3 identifier?)
     (exp3 expression?)
     (body expression?)
     ;; 这里多传一个env是因为我们实现的是let而非let*
     (saved-env environment?)
     (saved-env2 environment?)
     (saved-cont continuation?))
    (let3^3-cont
     (var3 identifier?)
     (body expression?)
     (saved-env environment?)
     (saved-cont continuation?))
    )

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  (define-datatype proc proc?
    (procedure
      (bvar symbol?)
      (body expression?)
      (env environment?)))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

  (define-datatype environment environment?
    (empty-env)
    (extend-env
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env-rec
      (p-name symbol?)
      (b-var symbol?)
      (p-body expression?)
      (saved-env environment?)))

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

)
