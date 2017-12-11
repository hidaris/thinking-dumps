#lang racket

(define g
  (let ([counter 0])
    (lambda (dummy)
      (begin
        (set! counter (+ counter 1))
        counter))))

(- (g 11) (g 11)) ; return -1

(define f
  (lambda (dummy)
    (let ([counter 0])
      (begin
        (set! counter (+ counter 1))
        counter))))

(- (f 11) (f 11)) ; return 0

;; why?
;; because in proc f, counter is at λ's scope
;; a.k.a local variable(more precisely: reference)
