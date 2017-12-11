(module data-structures (lib "eopl.ss" "eopl")

  ;; data structures for letrec-lang.

  (require "lang.rkt")                  ; for expression?

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

  ;; expval->num : ExpVal -> Int
  (define expval->num
    (lambda (v)
      (cases expval v
        (num-val (num) num)
        (else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  (define expval->bool
    (lambda (v)
      (cases expval v
        (bool-val (bool) bool)
        (else (expval-extractor-error 'bool v)))))

  ;; expval->proc : ExpVal -> Proc
  (define expval->proc
    (lambda (v)
      (cases expval v
        (proc-val (proc) proc)
        (else (expval-extractor-error 'proc v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                  variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  ;; proc? : SchemeVal -> Bool
  ;; procedure : Var * Exp * Env -> Proc
  (define-datatype proc proc?
    (procedure
        (bvar symbol?)
      (body expression?)
      (env environment?)))

  ;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

  ;; example of a data type built without define-datatype

  (define empty-env-record
    (lambda ()
      '()))

  (define extended-env-record
    (lambda (sym val old-env)
      (cons (list sym val) old-env)))

  (define extended-rec-env-record
    (lambda (pname bvar pbody old-env)
      (cons (list pname bvar pbody) old-env)))

  ;; predicates
  (define empty-env-record? null?)

  (define extended-env-record?
    (lambda (env)
      (eqv? (length (car env)) 2)))

  (define extended-rec-env-record?
    (lambda (env)
      (eqv? (length (car env)) 3)))

  (define environment?
    (lambda (x)
      (cond
        [(empty-env-record? x) #t]
        [(extended-env-record? x)
         (and (pair? x)
              (symbol? (car (car x)))
              (expval? (cadr (car x)))
              (environment? (cdr x)))]
        [(extended-rec-env-record? x)
         (and (pair? x)
              (symbol? (car (car x)))
              (symbol? (cadr (car x)))
              (expression? (caddr (car x)))
              (environment? (cdr x)))])))

  (define extended-env-record->sym
    (lambda (r)
      (car (car r))))

  (define extended-env-record->val
    (lambda (r)
      (cadr (car r))))

  (define extended-env-record->old-env
    (lambda (r)
      (cdr r)))

  (define extended-rec-env-record->pname
    (lambda (r)
      (car (car r))))

  (define extended-rec-env-record->bvar
    (lambda (r)
      (cadr (car r))))

  (define extended-rec-env-record->pbody
    (lambda (r)
      (caddr (car r))))

  (define extended-rec-env-record->old-env
    (lambda (r)
      (cdr r)))

  )
