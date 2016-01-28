#lang racket

(define member?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? a (car lat))
                (member? a (cdr lat)))])))

(member? 'sardines
         '(Italian sardines spaghetti parsley))

;; (define two-in-a-row?
;;   (lambda (lat)
;;     (cond
;;      ((null? lat) #f)
;;      (else
;;       (or (is-first? (car lat) (cdr lat))
;;           (two-in-a-row? (cdr lat)))))))

;; (define is-first?
;;   (lambda (a lat)
;;     (cond
;;      ((null? lat) #f)
;;      (else (eq? (car lat) a)))))

(define two-in-a-row?
  (lambda (lat)
    (cond
      [(null? lat) #f]
      [else
       (is-first-b? (car lat) (cdr lat))])))

(define is-first-b?
  (lambda (a lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? (car lat) a)
                (two-in-a-row? lat))])))

(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      [(null? lat) #f]
      [else (or (eq? (car lat) preceding)
                (two-in-a-row-b? (car lat)
                                 (cdr lat)))])))

(define two-in-a-row?
  (lambda (lat)
    (cond
      [(null? lat) #f]
      [else (two-in-a-row-b? (car lat)
                             (cdr lat))])))
(two-in-a-row? '(b d e i i a g))

(define sum-of-prefixes
  (lambda (tup)
    (cond
      [(null? tup) (quote ())]
      [else (sum-of-prefixes-b 0 tup)])))

(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
      [(null? tup) (quote ())]
      [else (cons (+ sonssf (car tup))
                  (sum-of-prefixes-b
                   (+ sonssf (car tup))
                   (cdr tup)))])))
;; sonssf is sum of numbers seen so far.
(sum-of-prefixes-b 0 '(1 1 1))
;; the eleventh commandment
;; use additional arguments when a function needs to know what other
;; arguments to the function have been like so far.

(define pick
  (lambda (n lat)
    (cond
      [(one? n) (car lat)]
      [else (pick (sub1 n) (cdr lat))])))

(define one?
  (lambda (n)
    (zero? (sub1 n))))

(define scramble-b
  (lambda (tup rev-pre)
    (cond
      [(null? tup) (quote ())]
      [else
       (cons (pick (car tup)
                   (cons (car tup) rev-pre))
             (scramble-b (cdr tup)
                         (cons (car tup) rev-pre)))])))

(define scramble
  (lambda (tup)
    (scramble-b tup (quote ()))))

;; pick rev-pre tup:(cdr tup)
;; 1    (1)     (1 1 3 4 2 1 1 9 2)
;; 1    (1 1)     (1 3 4 2 1 1 9 2)
;; 1    (1 1 1)    (3 4 2 1 1 9 2)
;; 1    (3 1 1 1)   (4 2 1 1 9 2)
;; 1    (4 3 1 1 1)   (2 1 1 9 2)
;; 4    (2 4 3 1 1 1)   (1 1 9 2)
;; 1    (1 2 4 3 1 1 1)  (1 9 2)
;; 1    (1 1 2 4 3 1 1 1)  (9 2)
;; 1    (9 1 1 2 4 3 1 1 1)  (2)
;; 9    (2 9 1 1 2 4 3 1 1 1)  ()
(scramble '(1 1 1 3 4 2 1 1 9 2))
