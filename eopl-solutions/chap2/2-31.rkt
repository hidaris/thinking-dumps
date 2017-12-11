#lang eopl

(require "base.ss")

(define-datatype prefix-list prefix-list?
  (a-prefix-list
   (exp prefix-exp?)
   (rest (list-of prefix-exp?))))

(define-datatype prefix-exp prefix-exp?
  (const-exp
   (num integer?))
  (diff-exp
   (operand1 prefix-exp?)
   (operand2 prefix-exp?)))

;; ; (- - 3 2 - 4 - 12 7)
(define parse-pexp
  (lambda (pexp)
    (cond
      ((integer? pexp) (const-exp pexp))
      ((list? pexp)
       (if (eqv? (car pexp) '-)
           (if (eqv? (length pexp) 3)
               (diff-exp
                (parse-pexp (list-ref pexp 1))
                (parse-pexp (list-ref pexp 2)))
               3)
           (eopl:error 'parse-pexp
                       "invalid prefix-exp")))
      (else
       (eopl:error 'parse-pexp
                   "not a prefix-exp")))))
