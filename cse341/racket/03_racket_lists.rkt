#lang racket

(provide (all-defined-out))

; list processing: null, cons, null?, car, cdr
; we won't use pattern-matching in racket

; sum of the numbers in a list
(define sum
  (lambda (xs)
    (cond
      ((null? xs) 0)
      (else (+ (car xs)
               (sum (cdr xs)))))))

; append
(define my-append
  (lambda (xs ys)
    (cond
      ((null? xs) ys)
      (else (cons (car xs)
                  (my-append (cdr xs) ys))))))

; map
(define my-map
  (lambda (f xs)
    (cond
      ((null? xs) '())
      (else (cons (f (car xs))
                  (my-map f (cdr xs)))))))