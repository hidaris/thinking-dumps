#lang typed/racket

(require typed/rackunit
         "interp.rkt"
         "utils.rkt"
         "tests.rkt")

(: get-name ((List Symbol String Any) -> String))
(define (get-name item)
  (match item
    [`(,name ,test ,result) (symbol->string name)]))

(: get-test ((List Symbol String Any) -> String))
(define (get-test item)
  (match item
    [`(,name ,test ,result) test]))

(: get-result ((List Symbol String Any) -> Any))
(define (get-result item)
  (match item
    [`(,name ,test ,result) result]))

(: test-all (-> Void))
(define (test-all)
  (for ([test-item : (List Symbol String Any) test-list])
    (let ([the-name (get-name test-item)])
      ;; if answer is error, check it and report!
      (if (eq? (get-result test-item) 'error)
          (check-not-exn
           (λ ()
             (run (get-test test-item)))
           the-name)
          (let* ([the-test (run (get-test test-item))]
                 [test-value (val->sval the-test)]
                 [the-answer (get-result test-item)])
            (check-equal? test-value
                          the-answer
                          the-name))))))
