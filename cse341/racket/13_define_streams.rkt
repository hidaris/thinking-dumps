#lang racket

; 1 1 1 1 1 ....

(define ones
  (lambda ()
    (cons 1 ones)))
; (define ones-really-bad (cons 1 ones-really-bad))
; (define ones-bad (lambda () (cons 1 (ones-bad))))

; 1 2 3 4 5

(define (f x)
  (lambda ()
    (cons x
          (f (+ x 1)))))

(define nats
  (lambda ()
    (f 1)))

(define nats2
  (letrec ([f (lambda (x)
                (cons x
                      (lambda ()
                        (f (+ x 1)))))])
    (lambda () (f 2))))

; 2 4 8 16 ...
(define powers-of-two
  (letrec ([f (lambda (x)
                (cons x
                      (lambda ()
                        (f (* x 2)))))])
    (lambda () (f 2))))