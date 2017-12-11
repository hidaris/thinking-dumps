#lang eopl

(require "base.ss")

(define identifier?
  (lambda (x)
    (and (symbol? x)
         (not (eqv? 'lambda x)))))

(define list-of
  (lambda (pred)
    (lambda (val)
      (or (null? val)
          (and (pair? val)
               (pred (car val))
               ((list-of pred) (cdr val)))))))

;;; (define-datatype type-name type-predicate-name
;;;  {(variant-name {(field-name predicate)}*)}+)
(define-datatype lc-exp lc-exp?
  (var-exp
   (var identifier?))
  (lambda-exp
   (bound-var (list-of identifier?))
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand (list-of lc-exp?))))

;;; parse-expression : SchemeVal -> LcExp
(define parse-expression
  (lambda (datum)
    (cond
      ((symbol? datum) (var-exp datum))
      ((pair? datum)
       (if (eqv? (car datum) 'lambda)
           (parse-lambda datum)
           (parse-app datum)))
      (else (report-invalid-concrete-syntax datum)))))

(define parse-lambda
  (lambda (datum)
    (cond
      ((null? (cdr datum))
       (eopl:error 'parse-lambda
                   "Missing bound variable and body."))
      ((not ((list-of identifier?) (cadr datum)))
       (eopl:error 'parse-lambda
                   "Not a list of bound variables."))
      ((null? (cddr datum))
       (eopl:error 'parse-lambda
                   "lambda body can't be empty."))
      (else (lambda-exp
             (cadr datum)
             (parse-expression (caddr datum)))))))

(define parse-app
  (lambda (datum)
    (if (list? datum)
        (app-exp
         (parse-expression (car datum))
         (map parse-expression (cdr datum)))
        (eopl:error 'parse-app
                    "invalid application expression."))))

(define report-invalid-concrete-syntax
  (lambda (datum)
    (eopl:error 'parse-expression
                "invalid-concrete-syntax ~s." datum)))

;;; unparse-lc-exp : LcExp -> SchemeVal
(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
           (var-exp (var)
                    var)
           (lambda-exp (bound-var body)
                       (list 'lambda bound-var
                             (unparse-lc-exp body)))
           (app-exp (rator rand)
                    (cons
                     (unparse-lc-exp rator)
                     (map unparse-lc-exp rand))))))

(equal?? (unparse-lc-exp
          (parse-expression
           '(lambda (x y) (+ x y))))
         '(lambda (x y) (+ x y)))
