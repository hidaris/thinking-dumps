#lang racket

(define apply-k
  (lambda ()
    (match k*
      [`(*-inner-fib-k ,fib-sub1-n^ ,k^)
       (begin (set! k* k^)
              (set! v* (+ fib-sub1-n^ v*))
              (apply-k))]
      [`(*-outer-fib-k ,n^ ,k^)
       (begin (set! n* (sub1 (sub1 n^)))
              (set! k* (*-inner-fib-k v* k^))
              (fib-cps))]
      [`(init-k) v*])))


(define *-inner-fib-k
  (lambda (fib-sub1-n^ k^)
    `(*-inner-fib-k ,fib-sub1-n^ ,k^)))


(define *-outer-fib-k
  (lambda (n^ k^)
    `(*-outer-fib-k ,n^ ,k^)))

(define init-k
  (lambda ()
    `(init-k)))

(define k* 'k)
(define n* 'n)
(define v* 'v)

(define fib-cps
  (λ ()
    (cond
      [(zero? n*) (begin (set! v* 0)
                         (apply-k))]
      [(zero? (sub1 n*)) (begin (set! v* 1)
                                (apply-k))]
      [else (begin (set! k* (*-outer-fib-k n* k*))
                   (set! n* (sub1 n*))
                   (fib-cps))])))

(define fib-driver
  (lambda (n)
    (begin (set! n* n)
           (set! k* (init-k))
           (fib-cps))))


(define times-cps
  (λ (ls k)
    (cond
      [(null? ls) (app-k k 1)]
      [(zero? (car ls)) (app-k k 0)]
      [else (times-cps (cdr ls) (*-times-inner-k ls k))])))

(define empty-k
  (lambda ()
    `(empty-k)))

(define *-times-inner-k
  (lambda (ls k)
    `(*-times-inner-k ,ls ,k)))

(define app-k
  (lambda (k v)
    (match k
      [`(empty-k) v]
      [`(*-times-inner-k ,ls ,k)
       (app-k k (* (car ls) v))])))

;; bi-trampoline, see a8-2
