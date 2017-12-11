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
      (proc proc?)))

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

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
	variant value)))

;;;;;;;;;;;;;;;; continuations ;;;;;;;;;;;;;;;;

  ;; Page: 148
  (define identifier? symbol?)

  (define apply-cont
    (lambda (cont val)
      (cont val)))

  (define end-cont
    (lambda ()
      (lambda (val)
        (begin
            (eopl:printf
              "End of computation.~%")
            val))))

  (define zero1-cont
    (lambda (cont)
      (lambda (val)
        (apply-cont cont
            (bool-val
              (zero? (expval->num val)))))))

  (define let-exp-cont
    (lambda (var body env cont)
      (lambda (val)
        ;; value-of/k在这里无法导入，会循环引用
        (value-of/k body
         (extend-env var val env) cont))))

  (define if-test-cont
    (lambda (exp2 exp3 env cont)
      (lambda (val)
        (if (expval->bool val)
            (value-of/k exp2 env cont)
            (value-of/k exp3 env cont)))))

  (define diff1-cont
    (lambda (exp2 env cont)
      (lambda (val)
        (value-of/k exp2
            env (diff2-cont val cont)))))

  (define diff2-cont
    (lambda (val1 cont)
      (lambda (val)
        (let ((num1 (expval->num val1))
              (num2 (expval->num val)))
            (apply-cont cont
              (num-val (- num1 num2)))))))

  (define rator-cont
    (lambda (rand env cont)
      (lambda (val)
        (value-of/k rand env (rand-cont val cont)))))

  (define rand-cont
    (lambda (val1 cont)
      (lambda (val)
        (let ((proc1 (expval->proc val)))
          (apply-procedure/k proc1 val1 cont)))))

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

)
