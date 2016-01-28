#lang racket

(provide (all-defined-out))

; [first big difference from ML (and Java)] PARENS MATTER!!
(define fact
  (lambda (n)
    (cond
      ((= n 0) 1)
      (else (* n (fact (- n 1)))))))

(define fact2
  (lambda (n)
    (cond
      ((= n 0) (1))
      (else (* n (fact (- n 1)))))))

(define fact3
  (lambda (n)
    (cond
      ((= n 0) (1))
      (else (* n (fact3 (- n 1)))))))

; (define (fact2 n) (if = 0 1 (* n (fact2 (- n 1)))))
; (define fact3 (n) (if (= n 0) 1 (* n (fact (- n 1)))))

(define (fact4 n) (if (= n 0) 1 (* n fact4 (- n 1))))

(define (fact5 n) (if (= n 0) 1 (* n ((fact5) (- n 1)))))

(define (fact6 n) (if (= n 0) 1 (n * (fact6 (- n 1)))))