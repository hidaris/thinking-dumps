#lang racket

(provide (all-defined-out))

; [second big difference from ML (and Java) Dynamic typing]

; dynamic typing: can use values of any types anywhere
; e.g., a list that holds numbers or other lists -- with
;  either lists or numbers nested arbitrarily deeply

(define xs (list 4 5 6))
(define ys (list (list 4 (list 5 0)) 6 7 (list 8) 9 2 3 (list 01)))

(define sum1
  (lambda (xs)
    (cond
      ((null? xs) 0)
      (else (cond
              ((number? (car xs))
               (+ (car xs) (sum1 (cdr xs))))
              (else
               (+ (sum1 (car xs)) (sum1 (cdr xs)))))))))

(define sum2
  (lambda (xs)
    (cond
      ((null? xs) 0)
      (else (cond
              ((number? (car xs))
               (+ (car xs) (sum2 (cdr xs))))
              (else (cond
                      ((list? (car xs))
                       (+ (sum2 (car xs)) (sum2 (cdr xs))))
                      (else
                       (sum2 (cdr xs))))))))))
               
