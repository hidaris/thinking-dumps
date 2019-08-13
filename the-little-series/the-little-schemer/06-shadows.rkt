#lang racket/base

;; Use help functions to abstract from representations.

(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define ^
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (x n (^ n (sub1 m)))))))

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

(define x
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (x n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup)
               (addtup (cdr tup)))))))

(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) (quote +))
      (and (numbered? (car aexp))
           (numbered?
            (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) (quote x))
      (and (numbered? (car aexp))
           (numbered?
            (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) (quote ^))
      (and (numbered? (car aexp))
           (numbered?
            (car (cdr (cdr aexp)))))))))
;; (define numbered?
;;   (lambda (aexp)
;;     (cond
;;      ((atom? aexp) (number? aexp))
;;      (else
;;       (and (numbered? (car aexp))
;;            (numbered?
;;             (car (cdr (cdr aexp)))))))))
(car (cdr (cdr '(1 + (2 + 2)))))
(numbered? '(1 + (2 + (2 ^ 3))))

;; n + m
(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car (cdr nexp)) (quote +))
      (o+ (value (car nexp))
          (value (car (cdr (cdr nexp))))))
     ((eq? (car (cdr nexp)) (quote x))
      (x (value (car nexp))
         (value (car (cdr (cdr nexp))))))
     (else
      (^ (value (car nexp))
         (value
          (car (cdr (cdr nexp)))))))))
(value '(2 + (3 ^ 2)))

;; (+ n m)
(define value2
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (car nexp) (quote +))
      (o+ (value2 (car (cdr nexp)))
          (value2 (car (cdr (cdr nexp))))))
     ((eq? (car nexp) (quote x))
      (x (value2 (car (cdr nexp)))
         (value2 (car (cdr (cdr nexp))))))
     (else
      (^ (value2 (car (cdr nexp)))
         (value2 (car (cdr (cdr nexp)))))))))
(value2 '(+ (x 2 3) (^ 2 3)))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

;; add some abstract
(define value3
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp) (quote +))
      (o+ (value3 (1st-sub-exp nexp))
          (value3 (2nd-sub-exp nexp))))
     ((eq? (operator nexp) (quote x))
      (x (value3 (1st-sub-exp nexp))
         (value3 (2nd-sub-exp nexp))))
     (else
      (^ (value3 (1st-sub-exp nexp))
         (value3 (2nd-sub-exp nexp)))))))
(value3 '(x (+ 2 3) (^ 2 3)))

;; church-code
(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons (quote ()) n)))

(define zub1
  (lambda (n)
    (cdr n)))

;; changed,but only slightly.
(define oo+
  (lambda (n m)
    (cond
     ((sero? m) n)
     (else (edd1 (oo+ n (zub1 m)))))))

;; recall lat?
(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))
