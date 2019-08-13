#lang racket/base

(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))
(atom? -3)

(define add1
  (lambda (n)
    (+ n 1)))
(add1 3)

(define sub1
  (lambda (n)
    (- n 1)))
(sub1 0)

(zero? 0)
(zero? 1492)

(+ 46 12)

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))
(o+ 1 4)

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))
(o- 4 1)
(o- 4 5)

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (o+ (car tup)
               (addtup (cdr tup)))))))
(addtup '(1 2 3 4 5 7 8 9 10))

;; The Fourth Commandment
;; Always change at least one argument while recurring. It
;; must be changed to be closer to termination. The changing
;; argument must be tested in the termination condition:
;; when using cdr, test termination with null? and
;; when using sub1, test termination with zero?.

(define x
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (x n (sub1 m)))))))
(x 3 4)
(x 12 3)

;; The Fifth Commandment
;; When building a value with +, always use 0 for the value of the
;; terminating line, for adding 0 does not change the value of an
;; addition.

;; When building a value with x, always use 1 for the value of the
;; terminating line, for multiplying by 1 does not change the value
;; of a multiplication.

;; When building a value with cons, always consider () for the value
;; of the terminating line.

(define tup+
  (lambda (tup1 tup2)
    (cond
     ;;((and (null? tup1) (null? tup2))
      ;;'())
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else
      (cons (o+ (car tup1) (car tup2))
            (tup+ (cdr tup1)
                  (cdr tup2)))))))
(tup+ '(1 3 4)
      '(2 4 4))

(tup+ '(1 3 4)
      '(3 5 6 7))

(define o>
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (o> (sub1 n) (sub1 m))))))
(o> 3 2)
(o> 4 5)
(o> 4 4)

(define o<
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else
      (o< (sub1 n) (sub1 m))))))
(o< 4 6)
(o< 8 3)
(o< 6 6)

(define o=
  (lambda (n m)
    (cond
     ((zero? n) (zero? m))
     ((zero? m) #f)
     (else (o= (sub1 n) (sub1 m))))))
(o= 0 0)

;; rewrite, using < and >.
(define oo=
  (lambda (n m)
    (cond
     ((o> n m) #f)
     ((o< n m) #f)
     (else #t))))
(oo= 3 3)

(define ^
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (x n (^ n (sub1 m)))))))
(^ 2 3)

(define ???
  (lambda (n m)
    (cond
     ((< n m) 0)
     (else (add1 (??? (- n m) m))))))
(??? 300000 90)

(define new-length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (new-length (cdr lat)))))))
(new-length '(1 2 3))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))
(pick 3 '(a b c d))

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat)
                 (rempick (sub1 n)
                          (cdr lat)))))))
(rempick 3 '(a b c d))

(define no-number
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((number? (car lat))
             (no-number (cdr lat)))
            (else (cons (car lat)
                        (no-number
                         (cdr lat)))))))))
(no-number '(1 a 3 g d 3))

(define all-number
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((number? (car lat))
             (cons (car lat)
                   (all-number
                    (cdr lat))))
            (else (all-number (cdr lat))))))))
(all-number '(1 k 3 j h 4))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (= a1 a2))
     ((or (number? a1) (number? a2))
      #f)
     (else (eq? a1 a2)))))
(eqan? 'a 'a)

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     (else
      (cond
       ((eq? a (car lat))
        (add1 (occur a (cdr lat))))
       (else (occur a (cdr lat))))))))
(occur 'a '(a v a c a))

;; (define one?
;;   (lambda (n)
;;     (cond
;;      ((zero? n) #f)
;;      (else (zero? (sub1 n))))))
;; (define one?
;;   (lambda (n)
;;     (cond
;;      (else (= n 1)))))
(define one?
  (lambda (n)
    (= n 1)))
(one? 1)

(define new-rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat)
                 (new-rempick (sub1 n)
                              (cdr lat)))))))
(new-rempick 3 '(a b c d))
