#lang racket

;;; deep again
(define deep
  (lambda (m)
    (cond
      ((zero? m) (quote pizza))
      (else (cons (deep (sub1 m))
                  (quote ()))))))

;;; we should just define the function six-layers
;;; and use it to create the pizzas we want:
(define six-layers
  (lambda (p)
    (cons
     (cons
      (cons
       (cons
        (cons
         (cons p (quote ()))
         (quote ()))
        (quote ()))
       (quote ()))
      (quote ()))
     (quote ()))))

;;; But if we had started with (deep 4)
;;; Then we would have had to define four-layers to
;;; create these special pizzas.
(define four-layers
  (lambda (p)
    (cons
     (cons
      (cons
       (cons p (quote ()))
       (quote ()))
      (quote ()))
     (quote ()))))

(define toppings 0)
(define deepB
  (lambda (m)
    (cond
      ((zero? m)
       (let/cc jump
         (set! toppings jump)
         (quote pizza)))
      (else (cons (deepB (sub1 m))
                  (quote ()))))))
;;; The Twentieth Commandment
;;; When thinking about a value created with (letcc ...),
;;; write down the function that is equivalent but does not
;;; forget. Then, when you use it, remember to forget.

;;; try to cons something onto toppings
;;; This is a version of deep that uses a collector.
;;; It has been a long time since we saw collectors in chap8.
(define deep&co
  (lambda (m k)
    (cond
      ((zero? m) (k (quote pizza)))
      (else
       (deep&co (sub1 m)
                (lambda (x)
                  (k (cons x (quote ())))))))))

;;; there is a better way to describe the collector (deep&co 2 (λ (x) x))
;;; It is equivalent to two-layers.
(define two-layers
  (lambda (p)
    (cons
     (cons p (quote ()))
     (quote ()))))

;;; this function remembers the collector in toppings.
(define deep&coB
  (lambda (m k)
    (cond
      ((zero? m)
       (let ()
         (set! toppings k)
         (k (quote pizza))))
      (else
       (deep&coB (sub1 m)
                 (lambda (x)
                   (k (cons x (quote ())))))))))

;;; Beware of shadows
;;; shadows are close to the real thing, but we should
;;; not forget the defference between them and the real thing.

(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else
       (two-in-a-row-b? (car lat)
                        (cdr lat))))))

;;; helper
(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      (else
       (or (eq? (car lat) preceding)
           (two-in-a-row-b? (car lat)
                            (cdr lat)))))))

;;; better version.
(define two-in-a-row-B?
  (letrec
      ((W (lambda (preceding lat)
            (cond
              ((null? lat) #f)
              (else
               (let ((nxt (car lat)))
                 (or (eq? nxt preceding)
                     (W nxt
                        (cdr lat)))))))))
    (lambda (lat)
      (cond
        ((null? lat) #f)
        (else (W (car lat)
                 (cdr lat)))))))

;;; two-in-a-row-B? determines whether any atom occurs
;;; twice in a row in a list of atoms. like '(a b b c)
;;; or '(a a b c), '(a b c c).


;;; The function two-in-a-row*? processes a list of S-expressions
;;; and checks whether any atom occurs twice in a row, regardless
;;; of parentheses.

(define leave 0)
(define walk
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (leave (car l)))
      (else
       (let ()
         (walk (car l))
         (walk (cdr l)))))))

(define atom?
  (lambda (a)
    (and (not (null? a)) (not (pair? a)))))

;;; walk is the minor function lm in leftmost.
(define leftmost
  (lambda (l)
    (let/cc skip
      (letrec
          ((lm (lambda (l)
                 (cond
                   ((null? l) (quote ()))
                   ((atom? (car l))
                    (skip (car l)))
                   (else
                    (let ()
                      (lm (car l))
                      (lm (cdr l))))))))
        (lm l)))))

;;; lm searches a list of S-expressions from left to right
;;; for the first atom and then gives this atom to a value
;;; created by (let/cc ...).

;;; if we put the right kind of value into leave.
;;; walk is like leftmost.
(define start-it
  (lambda (l)
    (let/cc here
      (set! leave here) ; Okay, now leave would be a needle!
      (walk l))))

;;; before determining the value of (leave (car l))
;;; the function waddle should remember in fill what is left to do.
(define fill 0)
(define waddle
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (let ()
         (let/cc rest
           (set! fill rest)
           (leave (car l)))
         (waddle (cdr l))))
      (else (let ()
              (waddle (car l))
              (waddle (cdr l)))))))

(define start-it2
  (lambda (l)
    (let/cc here
      (set! leave here)
      (waddle l))))

(define rest1
  (lambda (x)
    (waddle
     '(()
       (cheerios
        (cheerios
         (spaghettios)))
       donuts))))

(define get-next
  (lambda (x)
    (let/cc here-again
      (set! leave here-again)
      (fill (quote go)))))

(define rest2
  (lambda (x)
    (waddle
     '(((cheerios
         (spaghettios)))
       donuts))))

(define get-first
  (lambda (l)
    (let/cc here
      (set! leave here)
      (waddle l)
      (leave (quote ())))))

(define two-in-a-row*?
  (lambda (l)
    (let ((fst (get-first l)))
      (if (atom? fst)
          (two-in-a-row-b*? fst)
          #f))))

(define two-in-a-row-b*?
  (lambda (a)
    (let ((n (get-next (quote go))))
      (if (atom? n)
          (or (eq? n a)
              (two-in-a-row-b*? n))
          #f))))

(define two-in-a-row-*b?
  (letrec
      ((T? (lambda (a)
             (let ((n (get-next 0)))
               (if (atom? n)
                   (or (eq? n a)
                       (T? n))
                   #f))))
       (get-next
        (lambda (x)
          (let/cc here-again
            (set! leave here-again)
            (fill (quote go)))))
       (fill (lambda (x) x))
       (waddle
        (lambda (l)
          (cond
            ((null? l) (quote ()))
            ((atom? (car l))
             (let ()
               (let/cc rest
                 (set! fill rest)
                 (leave (car l)))
               (waddle (cdr l))))
            (else (let ()
                    (waddle (car l))
                    (waddle (cdr l)))))))
       (leave (lambda (x) x)))
    (lambda (l)
      (let ((fst (let/cc here
                   (set! leave here)
                   (waddle l)
                   (leave (quote ())))))
        (if (atom? fst)
            (T? fst)
            #f)))))
