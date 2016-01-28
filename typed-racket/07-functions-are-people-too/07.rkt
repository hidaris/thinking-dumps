#lang typed/racket

;;; Some functions consume values of arrow type;
;;; Some produce values of arrow type.

(define-type Int Integer)
(define-type Bool Boolean)

(define (identity x) x)

(define (true-maker x) true)

(define-type bool-or-int (U Hot Cold))

(struct Hot ([v : Bool])
  #:transparent)
(struct Cold ([v : Int])
  #:transparent)

(define (hot-maker x) Hot)
;;; constructors are functions

(define (help f)
  (Hot
   (true-maker
    (if (true-maker 5)
        f
        true-maker))))

;;; consists of an int and a function that
;;; consume that values to produce the next chain.
(define-type chain Link)

(struct Link ([v : Int] [w : (-> Int chain)])
  #:transparent)
;;; type mismatch
;;; I'm confused about it
(: ints (-> Int chain))
(define (ints n)
  (Link (+ n 1) ints))

(: skips (-> Int chain))
(define (skips n)
  (Link (+ n 2) skips))

(: eq-int (-> Int Int Bool))
(define (eq-int n m)
  (= n m))

(: divides-evenly (-> Int Int Bool))
(define (divides-evenly n c)
  (eq-int (modulo n c) 0))

(: is-mod-5-or-7 (-> Int Bool))
(define (is-mod-5-or-7 n)
  (if (divides-evenly n 5)
      true
      (divides-evenly n 7)))

(: some-ints (-> Int Link))
(define (some-ints n)
  (if (is-mod-5-or-7 (+ n 1))
      (Link (+ n 1) some-ints)
      (some-ints (+ n 1))))

(: chain-item (-> Int Link Int))
(define (chain-item n l)
  (match l
    [(Link i f)
     (if (eq-int n i)
         i
         (chain-item (- n 1) (f i)))]))

(: is-prime (-> Int Bool))
(define (is-prime n)
  (letrec ((has-no-divisors
            : (-> Int Int Bool)
            (lambda (n c)
              (if (eq-int c 1)
                  true
                  (if (eq-int n c)
                      (has-no-divisors n (- c 1))
                      (if (divides-evenly n c)
                          false
                          (has-no-divisors n (- c 1))))))))
    (has-no-divisors n n)))

;;; another long chain link.
(: primes (-> Int Link))
(define (primes n)
  (if (is-prime n)
      (Link n primes)
      (primes (+ n 1))))

(: fibs (-> Int (-> Int chain)))
(define fibs
  (lambda (n)
    (lambda (m)
      (Link (+ n m) (fibs m)))))

(: fibs-1 (-> Int chain))
(define (fibs-1 m)
  (Link (+ m 1) (fibs m)))

(: fibs-2 (-> Int chain))
(define (fibs-2 m)
  (Link (+ m 2) (fibs m)))
