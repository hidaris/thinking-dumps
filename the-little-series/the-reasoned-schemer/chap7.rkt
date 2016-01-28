#lang racket
(require "mk.rkt")

;;; A Bit Too Much
;;; need review.

(define bit-xoro
  (λ (x y r)
    (conde
      [(== 0 x) (== 0 y) (== 0 r)]
      [(== 1 x) (== 0 y) (== 1 r)]
      [(== 0 x) (== 1 y) (== 1 r)]
      [(== 1 x) (== 1 y) (== 0 r)]
      [else fail])))

(define bit-xoro2
  (λ (x y r)
    (fresh (s t u)
      (bit-nando x y s)
      (bit-nando x s t)
      (bit-nando x s t)
      (bit-nando s y u)
      (bit-nando t u r))))

(define bit-nando
  (λ (x y r)
    (conde
      [(== 0 x) (== 0 y) (== 1 r)]
      [(== 1 x) (== 0 y) (== 1 r)]
      [(== 0 x) (== 1 y) (== 1 r)]
      [(== 1 x) (== 1 y) (== 0 r)]
      [else fail])))

(run* (s)
  (fresh (x y)
    (bit-xoro x y 0)
    (== `(,x ,y) s)))
; => '((0 0) (1 1))

(run* (s)
  (fresh (x y)
    (bit-xoro x y 1)
    (== `(,x ,y) s)))
; => '((1 0) (0 1))

(run* (s)
  (fresh (x y r)
    (bit-xoro x y r)
    (== `(,x ,y ,r) s)))
; => '((0 0 0) (1 0 1) (0 1 1) (1 1 0))

(define bit-ando
  (λ (x y r)
    (conde
      [(== 0 x) (== 0 y) (== 0 r)]
      [(== 1 x) (== 0 y) (== 0 r)]
      [(== 0 x) (== 1 y) (== 0 r)]
      [(== 1 x) (== 1 y) (== 1 r)]
      [else fail])))

(define bit-ando2
  (λ (x y r)
    (fresh (s)
      (bit-nando x y s)
      (bit-noto s r))))

(define bit-noto
  (λ (x r)
    (bit-nando x x r)))

(run* (s)
  (fresh (x y)
    (bit-ando x y 1)
    (== `(,x ,y) s)))
; => '((1 1))

(define half-addero
  (λ (x y r c)
    (all
     (bit-xoro x y r)
     (bit-ando x y c))))

(run* (r)
  (half-addero 1 1 r 1))
; => '(0)

(run* (s)
  (fresh (x y r c)
    (half-addero x y r c)
    (== `(,x ,y ,r ,c) s)))
; => '((0 0 0 0) (1 0 1 0) (0 1 1 0) (1 1 0 1))
; given the bits x, y, r, and c, half-addero
; satisfies x + y = r + 2.c

; given the bits b, x, y, r, and c, full-addero
; satisfies b + x + y = r + 2.c.
(define full-addero
  (λ (b x y r c)
    (fresh (w xy wz)
      (half-addero x y w xy)
      (half-addero w b r wz)
      (bit-xoro xy wz c))))

(run* (s)
  (fresh (r c)
    (full-addero 0 1 1 r c)
    (== `(,r ,c) s)))
; => '((0 1))

(run* (s)
  (fresh (r c)
    (full-addero 1 1 1 r c)
    (== `(,r ,c) s)))
; => '((1 1))

(run* (s)
  (fresh (b x y r c)
    (full-addero b x y r c)
    (== `(,b ,x ,y ,r ,c) s)))
;; '((0 0 0 0 0)
;;   (1 0 0 1 0)
;;   (0 1 0 1 0)
;;   (1 1 0 0 1)
;;   (0 0 1 1 0)
;;   (1 0 1 0 1)
;;   (0 1 1 0 1)
;;   (1 1 1 1 1))

(define build-num
  (λ (n)
    (cond
      [(zero? n) '()]
      [(and (not (zero? n)) (even? n))
       (cons 0
             (build-num (/ n 2)))]
      [(odd? n)
       (cons 1
             (build-num (/ (- n 1) 2)))])))
; we can rearrange the cond lines in any order
; this is called the non-overlapping property.

(define poso
  (λ (n)
    (fresh (a d)
      (== `(,a . ,d) n))))

(run* (q)
  (poso '(0 1 1))
  (== #t q))
; => '(#t)

(run* (q)
  (poso '(1))
  (== #t q))
; => '(#t)

(run* (q)
  (poso '())
  (== #t q))
; => '()

(run* (r)
  (poso r))
; => '((_.0 . _.1))

(define >1o
  (λ (n)
    (fresh (a ad dd) ; car cadr cddr
      (== `(,a ,ad . ,dd) n))))

(run* (q)
  (>1o '(0 1 1))
  (== #t q))
; => '(#t)

(run* (q)
  (>1o '(1))
  (== #t q))
; => '()

(run* (q)
  (>1o '())
  (== #t q))
; => '()

(run* (r)
  (>1o r))
; => '((_.0 _.1 . _.2))

(define width
  (λ (n)
    (cond
      [(null? n) 0]
      [(pair? n) (+ (width (cdr n)) 1)]
      [else 1])))

(define addero
  (lambda (d n m r)
    (condi
     ((== 0 d) (== '() m) (== n r))
     ((== 0 d) (== '() n) (== m r)
      (poso m))
     ((== 1 d) (== '() m)
      (addero 0 n '(1) r))
     ((== 1 d) (== '() n) (poso m)
      (addero 0 '(1) m r))
     ((== '(1) n) (== '(1) m)
      (fresh (a c)
        (== `(,a ,c) r)
        (full-addero d 1 1 a c)))
     ((== '(1) n) (gen-addero d n m r))
     ((== '(1) m) (>1o n) (>1o r)
      (addero d '(1) n r))
     ((>1o n) (gen-addero d n m r))
     (else fail))))

(define gen-addero
  (lambda (d n m r)
    (fresh (a b c e x y z)
      (== `(,a . ,x) n)
      (== `(,b . ,y) m) (poso y)
      (== `(,c . ,z) r) (poso z)
      (alli
       (full-addero d a b c e)
       (addero e x y z)))))

(run 22 (s)
  (fresh (x y r)
    (addero 0 x y r)
    (== `(,x ,y ,r) s)))
;; '((_.0 () _.0)
;;   (() (_.0 . _.1) (_.0 . _.1))
;;   ((1) (1) (0 1))
;;   ((1) (0 _.0 . _.1) (1 _.0 . _.1))
;;   ((0 _.0 . _.1) (1) (1 _.0 . _.1))
;;   ((1) (1 1) (0 0 1))
;;   ((0 1) (0 1) (0 0 1))
;;   ((1) (1 0 _.0 . _.1) (0 1 _.0 . _.1))
;;   ((1 1) (1) (0 0 1))
;;   ((1) (1 1 1) (0 0 0 1))
;;   ((1 1) (0 1) (1 0 1))
;;   ((1) (1 1 0 _.0 . _.1) (0 0 1 _.0 . _.1))
;;   ((1 0 _.0 . _.1) (1) (0 1 _.0 . _.1))
;;   ((1) (1 1 1 1) (0 0 0 0 1))
;;   ((0 1) (0 0 _.0 . _.1) (0 1 _.0 . _.1))
;;   ((1) (1 1 1 0 _.0 . _.1) (0 0 0 1 _.0 . _.1))
;;   ((1 1 1) (1) (0 0 0 1))
;;   ((1) (1 1 1 1 1) (0 0 0 0 0 1))
;;   ((0 1) (1 1) (1 0 1))
;;   ((1) (1 1 1 1 0 _.0 . _.1) (0 0 0 0 1 _.0 . _.1))
;;   ((1 1 0 _.0 . _.1) (1) (0 0 1 _.0 . _.1))
;;   ((1) (1 1 1 1 1 1) (0 0 0 0 0 0 1)))

(run* (s)
  (gen-addero 1 '(0 1 1) '(1 1) s))
; => '((0 1 0 1))

(run* (s)
  (fresh (x y)
    (addero 0 x y '(1 0 1))
    (== `(,x ,y) s)))
;; '(((1 0 1) ())
;;   (() (1 0 1))
;;   ((1) (0 0 1))
;;   ((0 0 1) (1))
;;   ((1 1) (0 1))
;;   ((0 1) (1 1)))

; use +o to generate the pairs of numbers
; that sum to five
(define +o
  (lambda (n m k)
    (addero 0 n m k)))

(run* (s)
  (fresh (x y)
    (+o x y '(1 0 1))
    (== `(,x ,y) s)))
;; '(((1 0 1) ())
;;   (() (1 0 1))
;;   ((1) (0 0 1))
;;   ((0 0 1) (1))
;;   ((1 1) (0 1))
;;   ((0 1) (1 1)))

(define _o
  (λ (n m k)
    (+o m k n)))

(run* (q)
  (_o '(0 0 0 1) '(1 0 1) q))
; => '((1 1))

(run* (q)
  (_o '(0 1 1) '(0 1 1) q))
; => '(())

(run* (q)
  (_o '(0 1 1) '(0 0 0 1) q))
; => '()
