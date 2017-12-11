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

; ((lambda (a) (a b)) c)
;                app-exp
;                /     \
;       lambda-exp     var-exp
;        /      \         |
;  bound-var    body      c
;      |         |
;      a      app-exp
;             /     \
;        var-exp   var-exp
;           |          |
;           a          b
(define exp1
  (app-exp (lambda-exp 'a
                       (app-exp (var-exp 'a) (var-exp 'b)))
           (var-exp 'c)))

; (lambda (x)
;   (lambda (y)
;     ((lambda (x)
;        (x y))
;      x)))
;       lambda-exp
;        /       \
;  bound-var    body
;      |         |
;      x     lambda-exp
;             /      \
;       bound-var    body
;           |         |
;           y      app-exp
;                  /     \
;         lambda-exp     var-exp
;          /      \         |
;    bound-var    body      x
;        |         |
;        x      app-exp
;               /     \
;          var-exp   var-exp
;             |          |
;             x          y
(define exp2
  (lambda-exp 'x
              (lambda-exp 'y
                          (app-exp (lambda-exp 'x
                                               (app-exp (var-exp 'x)
                                                        (var-exp 'y)))
                                   (var-exp 'x)))))

;;; unparse-lc-exp : LcExp -> SchemeVal
(define unparse-lc-exp
  (lambda (exp)
    (cases lc-exp exp
           (var-exp (var)
                    var)
           (lambda-exp (bound-var body)
                       (list 'lambda (list bound-var)
                             (unparse-lc-exp body)))
           (app-exp (rator rand)
                    (list
                     (unparse-lc-exp rator)
                     (unparse-lc-exp rand))))))

(equal?? (unparse-lc-exp exp1)
         '((lambda (a) (a b)) c))

(equal?? (unparse-lc-exp exp2)
         '(lambda (x)
            (lambda (y)
              ((lambda (x)
                 (x y))
               x))))
