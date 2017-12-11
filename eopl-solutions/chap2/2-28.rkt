#lang eopl

(require "base.ss")

(define identifier?
  (lambda (x)
    (and (symbol? x)
         (not (eqv? 'lambda x)))))

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

;;; parse-expression : SchemeVal -> LcExp
(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (lambda-exp
            (car (cadr datum))
            (parse-expression (caddr datum)))
           (app-exp
            (parse-expression (car datum))
            (parse-expression (cadr datum)))))
      (else (report-invalid-concrete-syntax datum)))))

(define report-invalid-concrete-syntax
  (lambda (datum)
    (eopl:error 'parse-expression
                "invalid-concrete-syntax ~s." datum)))

;;; unparse-lc-exp : LcExp -> SchemeVal
(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
           (var-exp (var)
                    (symbol->string var))
           (lambda-exp (bound-var body)
                       (string-append
                        "(lambda (" (symbol->string bound-var) ") "
                        (unparse-lc-exp body) ")"))
           (app-exp (rator rand)
                    (string-append
                     "("(unparse-lc-exp rator) " "
                     (unparse-lc-exp rand) ")")))))
