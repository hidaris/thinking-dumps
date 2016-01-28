#lang racket

(provide (all-defined-out))

;; this is a silly addition function that purposely runs slows for
;; demonstration purposes
(define (slow-add x y)
  (letrec ([slow-id (lambda (y z)
                      (if (= 0 z)
                          y
                          (slow-id y (- z 1))))])
    (+ (slow-id x 50000000) y)))

;; multiplies x and result of y-thunk, calling y-thunk x times
(define (my-mult x y-thunk)  ;; assumes x is >= 0
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [#t (+ (y-thunk) (my-mult (- x 1) y-thunk))]))

(define (my-delay th)
  (mcons #f th)) ;; a one-of "type" we will update /in place/

(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)
             (set-mcdr! p ((mcdr p)))
             (mcdr p))))