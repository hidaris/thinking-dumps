#lang racket/base

(require rackunit
         "ast.rkt"
         "parser.rkt"
         "env.rkt"
         "interp.rkt"
         "tests.rkt")

(define run
  (λ (str)
    (value-of-program (parse str))))

(define (test-all)
  (for ([test-item test-list])
    (let ([the-name (car test-item)])
      ;; if answer is error, check it and report!
      (if (eq? (caddr test-item) 'error)
          (check-not-exn
           (λ ()
             (run (cadr test-item)))
           the-name)
          (let* ([the-test (run (cadr test-item))]
                 [test-value (val->sval the-test)]
                 [the-answer (caddr test-item)])
            (check-equal? test-value
                          the-answer
                          the-name))))))
