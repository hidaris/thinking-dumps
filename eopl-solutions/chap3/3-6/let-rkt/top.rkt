#lang racket/base

(require rackunit
         "./parser.rkt"
         "./env.rkt"
         "./interp.rkt"
         "./tests.rkt")

(define run
  (lambda (sexp)
    (value-of (parse sexp) (init-env))))

(define (test-all)
  (for ([test-item test-list])
    (let ([the-name (car test-item)])
      ;; if answer is error, check it and report!
      (if (equal? (caddr test-item) 'error)
          (check-not-exn (lambda () (run (cadr test-item)))
                         the-name)
          (let ([the-test (run (cadr test-item))]
                [the-answer (run (caddr test-item))])
            (check-equal? the-test the-answer the-name))))))
