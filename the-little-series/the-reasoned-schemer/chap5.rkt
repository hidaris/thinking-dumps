#lang racket
(require "mk.rkt")

;;; Double your Fun
(define append
  (lambda (l s)
    (cond
      ((null? l) s)
      (else (cons (car l)
                  (append (cdr l) s))))))

(append '(a b c) '(d e))
; => '(a b c d e)

(append '(a b c) '())
; => '(a b c)

(append '() '(d e))
; => '(d e)

;; (append 'a '(d e))
; It has no meaning.

(append '(d e) 'a)
; => '(d e . a)

(define appendo
  (lambda (l s out)
    (conde
      ((nullo l) (== s out))
      (else
       (fresh (a d res)
         ;(caro l a)
         ;(cdro l d)
         (conso a d l)
         (conso a res out)
         (appendo d s res))))))

(run* (x)
  (appendo
   '(cake)
   '(tastes yummy)
   x))
; => '((cake tastes yummy))

(run* (x)
  (fresh (y)
    (appendo
     `(cake with ice ,y)
     '(tastes yummy)
     x)))
; => '((cake with ice _.0 tastes yummy))

(run* (x)
  (fresh (y)
    (appendo
     '(cake with ice cream)
     y
     x)))
; => '((cake with ice cream . _.0))

(run 1 (x)
  (fresh (y)
    (appendo `(cake with ice . ,y) '(d t) x)))
; => '((cake with ice d t))

;; we can show that y is associated with the empty list:
(run 1 (y)
  (fresh (x)
    (appendo `(cake with ice . ,y) '(d t) x)))
; => '(())

(run 5 (x)
  (fresh (y)
    (appendo `(cake with ice . ,y) '(d t) x)))
; => '((cake with ice d t)
;      (cake with ice _.0 d t)
;      (cake with ice _.0 _.1 d t)
;      (cake with ice _.0 _.1 _.2 d t)
;      (cake with ice _.0 _.1 _.2 _.3 d t))

(run 5 (y)
  (fresh (x)
    (appendo `(cake with ice . ,y) '(d t) x)))
; => '(() (_.0) (_.0 _.1) (_.0 _.1 _.2) (_.0 _.1 _.2 _.3))

(run 5 (x)
  (fresh (y)
    (appendo
     `(cake with ice . ,y)
     `(d t . ,y)
     x)))
; => '((cake with ice d t)
;      (cake with ice _.0 d t _.0)
;      (cake with ice _.0 _.1 d t _.0 _.1)
;      (cake with ice _.0 _.1 _.2 d t _.0 _.1 _.2)
;      (cake with ice _.0 _.1 _.2 _.3 d t _.0 _.1 _.2 _.3))

(run 5 (y)
  (fresh (x)
    (appendo
     `(cake with ice . ,y)
     `(d t . ,y)
     x)))
; => '(() (_.0) (_.0 _.1) (_.0 _.1 _.2) (_.0 _.1 _.2 _.3))

(run* (x)
  (fresh (z)
    (appendo
     `(cake with ice cream)
     `(d t . ,z)
     x)))
; => '((cake with ice cream d t . _.0))

(run 6 (x)
  (fresh (y)
    (appendo x y `(cake with ice d t))))
; => '(()
;      (cake)
;      (cake with)
;      (cake with ice)
;      (cake with ice d)
;      (cake with ice d t))

(run 6 (y)
  (fresh (x)
    (appendo x y `(cake with ice d t))))
; => '((cake with ice d t) (with ice d t) (ice d t) (d t) (t) ())

(run 6 (r)
  (fresh (x y)
    (appendo x y '(cake with ice d t))
    (== `(,x ,y) r)))
; =>'((() (cake with ice d t))
;     ((cake) (with ice d t))
;     ((cake with) (ice d t))
;     ((cake with ice) (d t))
;     ((cake with ice d) (t))
;     ((cake with ice d t) ()))

(run 7 (r)
  (fresh (x y)
    (appendo x y '(cake with ice d t))
    (== `(,x ,y) r)))
; =>'((() (cake with ice d t))
;     ((cake) (with ice d t))
;     ((cake with) (ice d t))
;     ((cake with ice) (d t))
;     ((cake with ice d) (t))
;     ((cake with ice d t) ()))
;; => it has no value. X, I'm confused about it.

(run 7 (x)
  (fresh (y z)
    (appendo x y z)))
;; '(()
;;   (_.0)
;;   (_.0 _.1)
;;   (_.0 _.1 _.2)
;;   (_.0 _.1 _.2 _.3)
;;   (_.0 _.1 _.2 _.3 _.4)
;;   (_.0 _.1 _.2 _.3 _.4 _.5))

(run 7 (y)
  (fresh (x z)
    (appendo x y z)))
; => '(_.0 _.0 _.0 _.0 _.0 _.0 _.0)

(run 7 (z)
  (fresh (x y)
    (appendo x y z)))
;; '(_.0
;;   (_.0 . _.1)
;;   (_.0 _.1 . _.2)
;;   (_.0 _.1 _.2 . _.3)
;;   (_.0 _.1 _.2 _.3 . _.4)
;;   (_.0 _.1 _.2 _.3 _.4 . _.5)
;;   (_.0 _.1 _.2 _.3 _.4 _.5 . _.6))

(run 7 (r)
  (fresh (x y z)
    (appendo x y z)
    (== `(,x ,y ,z) r)))
; '((() _.0 _.0)
;   ((_.0) _.1 (_.0 . _.1))
;   ((_.0 _.1) _.2 (_.0 _.1 . _.2))
;   ((_.0 _.1 _.2) _.3 (_.0 _.1 _.2 . _.3))
;   ((_.0 _.1 _.2 _.3) _.4 (_.0 _.1 _.2 _.3 . _.4))
;   ((_.0 _.1 _.2 _.3 _.4) _.5 (_.0 _.1 _.2 _.3 _.4 . _.5))
;   ((_.0 _.1 _.2 _.3 _.4 _.5) _.6 (_.0 _.1 _.2 _.3 _.4 _.5 . _.6)))

;;; Define swappendo, which is just appendo
;;; with its two conde lines swapped.
;; (define swappendo
;;   (lambda (l s out)
;;     (conde
;;      (succeed
;;       (fresh (a d res)
;;         (conso a d l)
;;         (conso a res out)
;;         (swappendo d s res)))
;;      (else (nullo l) (== s out)))))

(define-syntax lambda-limited
  (syntax-rules ()
    ((_ n formals g)
     (let ((x (var 'x)))
       (lambda formals
         (ll n x g))))))

(define ll
  (lambda (n x g)
    (lambdag@ (s)
              (let ((v (walk x s)))
                (cond
                  ((var? v) (g (ext-s x 1 s)))
                  ((< v n) (g (ext-s (+ v 1) s)))
                  (else (fail s)))))))

(define swappendo
  (lambda-limited 5 (l s out)
                  (conde
                    (succeed (fresh (a d res)
                               (conso a d l)
                               (conso a res out)))
                    (else (nullo l) (== s out)))))

;; according to the TRS, this shouldn't have a value...
(run 1 (z)
  (fresh (x y)
    (swappendo x y z)))

;;; Consider this definition
(define unwrap
  (lambda (x)
    (cond
      ((pair? x) (unwrap (car x)))
      (else x))))

(unwrap '((((pizza)))))
; => 'pizza

(unwrap '(((pizza pie)) extra cheese))
; => 'pizza

;; Define unwrapo
;; (define unwrapo
;;   (lambda (x out)
;;     (conde
;;      ((pairo x)
;;       (fresh (a)
;;         (caro x a)
;;         (unwrapo a out)))
;;      (else (== x out)))))
;;; unwrapo recursion too early, so swap it.

(define unwrapo
  (lambda (x out)
    (conde
      (succeed (== x out))
      ((fresh (a)
         (caro x a)
         (unwrapo a out))))))

(run* (x)
  (unwrapo '(((pizza))) x))
; => '((((pizza))) ((pizza)) (pizza) pizza)

(run 1 (x)
  (unwrapo x 'pizza))
; => '(pizza)

(run 1 (x)
  (unwrapo `((,x)) 'pizza))
; => '(pizza)

(run 5 (x)
  (unwrapo x 'pizza))
; '(pizza
;   (pizza . _.0)
;   ((pizza . _.0) . _.1)
;   (((pizza . _.0) . _.1) . _.2)
;   ((((pizza . _.0) . _.1) . _.2) . _.3))

(run 5 (x)
  (unwrapo x '((pizza))))
; '(((pizza))
;   (((pizza)) . _.0)
;   ((((pizza)) . _.0) . _.1)
;   (((((pizza)) . _.0) . _.1) . _.2)
;   ((((((pizza)) . _.0) . _.1) . _.2) . _.3))

(run 5 (x)
  (unwrapo `((,x)) 'pizza))
; '(pizza
;   (pizza . _.0)
;   ((pizza . _.0) . _.1)
;   (((pizza . _.0) . _.1) . _.2)
;   ((((pizza . _.0) . _.1) . _.2) . _.3))

(define flatten
  (lambda (s)
    (cond
      ((null? s) '())
      ((pair? s)
       (append
        (flatten (car s))
        (flatten (cdr s))))
      (else (cons s '())))))

(flatten '((a b) c))
; => '(a b c)

(define flatteno
  (lambda (s out)
    (conde
      ((nullo s) (== '() out))
      ((pairo s)
       (fresh (a d res-a res-d)
         (conso a d s)
         (flatteno a res-a)
         (flatteno d res-d)
         (appendo res-a res-d out)))
      (else (conso s '() out)))))

(run 1 (x)
  (flatteno '((a b) c) x))
; => '((a b c))

(run 1 (x)
  (flatteno '(a (b c)) x))
; => '((a b c))

(run* (x)
  (flatteno '(a) x))
; => '((a) (a ()) ((a)))

(run* (x)
  (flatteno '((a)) x))
; => '((a) (a ()) (a ()) (a () ()) ((a)) ((a) ()) (((a))))

(run* (x)
  (flatteno '(((a))) x))
;; '((a)
;;   (a ())
;;   (a ())
;;   (a () ())
;;   (a ())
;;   (a () ())
;;   (a () ())
;;   (a () () ())
;;   ((a))
;;   ((a) ())
;;   ((a) ())
;;   ((a) () ())
;;   (((a)))
;;   (((a)) ())
;;   ((((a)))))

(run* (x)
  (flatteno '((a b) c) x))
;; '((a b c)
;;   (a b c ())
;;   (a b (c))
;;   (a b () c)
;;   (a b () c ())
;;   (a b () (c))
;;   (a (b) c)
;;   (a (b) c ())
;;   (a (b) (c))
;;   ((a b) c)
;;   ((a b) c ())
;;   ((a b) (c))
;;   (((a b) c)))

;; (run* (x)
;;   (flatteno x '(a b c)))
;; It has no value.

(define flattenrevo
  (lambda (s out)
    (conde
      (succeed (conso s '() out))
      ((nullo s) (== '() out))
      (else
       (fresh (a d res-a res-d)
         (conso a d s)
         (flattenrevo a res-a)
         (flattenrevo d res-d)
         (appendo res-a res-d out))))))

(run* (x)
  (flattenrevo '((a b) c) x))
;; '((((a b) c))
;;   ((a b) (c))
;;   ((a b) c ())
;;   ((a b) c)
;;   (a (b) (c))
;;   (a (b) c ())
;;   (a (b) c)
;;   (a b () (c))
;;   (a b () c ())
;;   (a b () c)
;;   (a b (c))
;;   (a b c ())
;;   (a b c))

(reverse
 (run* (x)
   (flattenrevo '((a b) c) x)))
;; '((a b c)
;;   (a b c ())
;;   (a b (c))
;;   (a b () c)
;;   (a b () c ())
;;   (a b () (c))
;;   (a (b) c)
;;   (a (b) c ())
;;   (a (b) (c))
;;   ((a b) c)
;;   ((a b) c ())
;;   ((a b) (c))
;;   (((a b) c)))

(run 2 (x)
  (flattenrevo x '(a b c)))
; => '((a b . c) (a b c))

;; (run 3 (x)
;;      (flattenrevo x '(a b c)))
;; it has no value.

(length
 (run* (x)
   (flattenrevo '((((a (((b))) c))) d) x)))
; => 574
