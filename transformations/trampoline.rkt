#lang racket
(require racket/trace)

(define empty-k
  (lambda ()
    `(empty-k)))

(define big-k
  (lambda (n k)
    `(big-k ,n ,k)))

(define little-k
  (lambda (w k)
    `(little-k ,w ,k)))

(define n* 'n)
(define k* 'k)
(define v* 'v)
(define pc* 'pc)

(define fib
  (lambda ()
    (cond
      [(zero? n*) (begin (set! v* 0) (apply-k))]
      [(= n* 1) (begin (set! v* 1) (apply-k))]
      [else (begin
              (set! k* (big-k n* k*))
              (set! n* (- n* 1))
              (set! pc* fib))])))
(define done* 'done*)
(define apply-k
  (lambda ()
    (match k*
      [`(empty-k) (set! done* #t)]
      [`(big-k ,n ,k)
       (begin
         (set! n* (- n 2))
         (set! k* (little-k v* k))
         (set! pc* fib))]
      [`(little-k ,w ,k)
       (begin
         (set! v* (+ w v*))
         (set! k* k)
         (set! pc* apply-k))])))

(define fib-driver
  (lambda (n)
    (begin
      (set! n* n)
      (set! k* (empty-k))
      (set! done* #f)
      (set! pc* fib)
      (trampoline))))

(define trampoline
  (lambda ()
    (if done*
        v*
        (begin (pc*) (trampoline)))))

(trace fib)