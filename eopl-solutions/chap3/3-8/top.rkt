#lang racket/base

(require rackunit
         "./ast.rkt"
         "./parser.rkt"
         "./env.rkt"
         "./interp.rkt"
         "./tests.rkt")

(define run
  (lambda (str)
    (value-of-program (parse str))))

(define (test-all)
  (for ([test-item test-list])
    (let ([the-name (car test-item)])
      ;; if answer is error, check it and report!
      (if (equal? (caddr test-item) 'error)
          (check-not-exn (lambda () (run (cadr test-item)))
                         the-name)
          (let* ([the-test (run (cadr test-item))]
                 [test-value (val->sval the-test)]
                 [the-answer (caddr test-item)])
            (check-equal? test-value the-answer the-name))))))
