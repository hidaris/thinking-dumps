#lang racket/base

(require rackunit
         "./ast.rkt"
         "./parser.rkt"
         "./env.rkt"
         "./utils.rkt"
         "./interp.rkt"
         "./tests.rkt")

;;;;; for test ;;;;;
(define get-exp cadr)
(define get-answer caddr)
(define get-name car)

(define run
  (lambda (str)
    (value-of-program (parse str))))

(define (test-all)
  (for ([test-item test-list])
    (let ([name (get-name test-item)]
          [answer (get-answer test-item)])
      ;; if answer is error, check it and report!
      (if (equal? answer 'error)
          (check-not-exn (λ ()
                           (run (get-exp test-item))) name)
          (let* ([exp (run (get-exp test-item))]
                 [value (val->sval exp)])
            (check-equal? value answer name))))))
