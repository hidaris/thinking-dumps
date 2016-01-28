#lang racket

(define pr
  (cons 1 (cons #t "hi")))

(define 1st
  (cons 1 (cons #t (cons "hi" null))))

(define x (cons (cons #t null) null))
(cdr (car x))
