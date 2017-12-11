#lang eopl

(define identifier?
  (lambda (var)
    (if (eqv? var 'lambda)
        (eopl:error 'identifier?
                    "var should be symbol other than lambda")
        (symbol? var))))

;;; (define-datatype type-name type-predicate-name
;;;  {(variant-name {(field-name predicate)}*)}+)
(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var identifier?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))
