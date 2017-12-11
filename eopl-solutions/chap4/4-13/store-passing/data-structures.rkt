(module data-structures (lib "eopl.ss" "eopl")

  (require "lang.rkt")                  ; for expression?

  (provide (all-defined-out))           ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean, a procval, or a
;;; reference.

  (define-datatype expval expval?
    (num-val
     (value number?))
    (bool-val
     (boolean boolean?))
    (proc-val
     (proc proc?))
    (ref-val
     (ref reference1?))
    (list-val
     (values (list-of expval?))))

  (define-datatype answer answer?
    (an-answer
     (value expval?)
     (store store1?)))

  (define reference1?
    (lambda (v)
      (integer? v)))

  (define store1?
    (lambda (v)
      (vector? v)))


;;; extractors:

  (define answer->value
    (lambda (v)
      (cases answer v
        (an-answer (value store) value))))

  (define answer->store
    (lambda (v)
      (cases answer v
        (an-answer (value store) store))))

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

  (define expval->ref
    (lambda (v)
      (cases expval v
        (ref-val (ref) ref)
        (else (expval-extractor-error 'reference v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                  variant value)))

;;;;;;;;;;;;;;;; procedures ;;;;;;;;;;;;;;;;

  (define-datatype proc proc?
    (procedure
      (bvar (list-of symbol?))
      (body expression?)
      (env environment?)))

  (define-datatype environment environment?
    (empty-env)
    (extend-env
      (bvar symbol?)
      (bval expval?)
      (saved-env environment?))
    (extend-env-rec*
      (proc-names (list-of symbol?))
      (b-vars (list-of (list-of symbol?)))
      (proc-bodies (list-of expression?))
      (saved-env environment?)))

  ;; env->list : Env -> List
  ;; used for pretty-printing and debugging
  (define env->list
    (lambda (env)
      (cases environment env
        (empty-env () '())
        (extend-env (sym val saved-env)
          (cons
           (list sym (expval->printable val))
           (env->list saved-env)))
        (extend-env-rec* (p-names b-vars p-bodies saved-env)
          (cons
           (list 'letrec p-names '...)
           (env->list saved-env))))))

  ;; expval->printable : ExpVal -> List
  ;; returns a value like its argument, except procedures get cleaned
  ;; up with env->list
  (define expval->printable
    (lambda (val)
      (cases expval val
        (proc-val (p)
          (cases proc p
            (procedure (var body saved-env)
              (list 'procedure var '... (env->list saved-env)))))
        (else val))))
  )