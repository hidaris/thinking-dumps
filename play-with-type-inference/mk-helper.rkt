#lang racket
(require minikanren)
(provide (all-defined-out))

;; helper
(define nullo
  (lambda (x)
    (== '() x)))

(define caro
  (lambda (p a)
    (fresh (d)
      (== (cons a d) p))))

(define cdro
  (lambda (p d)
    (fresh (a)
      (== (cons a d) p))))

(define eq-caro
  (lambda (l x)
    (caro l x)))

(define booleano
  (lambda (x)
    (conde
      [(== #f x) succeed]
      [(== #t x) succeed])))

(define membero
  (lambda (x l)
    (conde
      ((nullo l) fail)
      ((eq-caro l x) succeed)
      ((fresh (d)
         (cdro l d)
         (membero x d))))))
