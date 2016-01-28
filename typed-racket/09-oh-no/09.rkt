#lang typed/racket

(define-type (list a)
  (U Empty
     (Cons a)))

(struct Empty () #:transparent)
(struct (a) Cons ([v : a] [w : (list a)]) #:transparent)

(define-type box
  (U Bacon
     Ix))

(struct Bacon () #:transparent)
(struct Ix ([v : Integer]) #:transparent)

(: is-bacon : (box -> Boolean))
(define (is-bacon box)
  (match box
    [(Bacon) true]
    [(Ix n) false]))

;; (struct (No-bacon exn:fail:user) ())
(struct No-bacon exn:fail () #:transparent)

(: where-is : ((list box) -> Integer))
(define (where-is list)
  (match list
    [(Empty)
     (raise (No-bacon
             "failed"
             (current-continuation-marks)))]
    [(Cons a-box rest)
     (if (is-bacon a-box)
         1
         (+ 1 (where-is rest)))]))

(: eq-int : (Number Number -> Boolean))
(define (eq-int n1 n2)
  (= n1 n2))

(define-struct (Out-of-range exn:fail:user) ())

(: list-item : (Number (list box) -> box))
(define (list-item n list)
  (match list
    [(Empty)
     (raise (Out-of-range
             "failed"
             (current-continuation-marks)))]
    [(Cons abox rest)
     (if (eq-int n 1)
         abox
         (list-item (- n 1) rest))]))

(: find : (Number (list box) -> Number))
(define (find n boxes)
  (letrec ([check
            : (Number (list box) box -> Number)
            (lambda (n boxes box)
              (match box
                [(Bacon) n]
                [(Ix i) (find i boxes)]))])
    (with-handlers ([Out-of-range?
                     (lambda (v)
                       (find (/ n 2) boxes))])
      (check n boxes (list-item n boxes)))))

(: path : (Number (list box) -> (list Number)))
(define (path n boxes)
  (letrec ([check
            : ((list box) box -> (list Number))
            (lambda (boxes box)
              (match box
                [(Bacon) (Empty)]
                [(Ix i) (path i boxes)]))])
    (Cons n (with-handlers ([Out-of-range?
                             (lambda (v)
                               (path (/ n 2) boxes))])
              (check boxes (list-item n boxes))))))
