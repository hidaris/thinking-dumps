#lang typed/racket

;; we represent a node in a binomial tree as
;; an element and a list of children.
(define-type (Tree a) (Node a))
(struct (a) Node ([rank : Integer]
                  [elem : a]
                  [tree : (Listof (Tree a))])
  #:transparent)

(define-type Order (U LT EQ GT))
(struct LT () #:transparent)
(struct EQ () #:transparent)
(struct GT () #:transparent)

;; We always link trees of equal rank
(: link : (∀ (a)
             (a a -> Order) (Tree a) (Tree a) -> (Tree a)))
(define (link f t1 t2)
  (match* (t1 t2)
    [((Node r x1 c1) (Node _ x2 c2))
     (match (f x1 x2)
       [(LT) (Node (add1 r) x1 (cons t2 c1))]
       [else (Node (add1 r) x2 (cons t1 c2))])]))

(define-type (Heap a) (Listof (Tree a)))

(: rank : (∀ (a) (Tree a) -> Integer))
(define (rank h)
  (match h
    [(Node r x c) r]))

(: insTree : (∀ (a) (a a -> Order) (Tree a) (Heap a) -> (Heap a)))
(define (insTree f t ts)
  (match ts
    ['() (cons t '())]
    [`(,t1 . ,ts1)
     (if (< (rank t) (rank t1))
         (cons t ts)
         (insTree f (link f t t1) ts1))]))

(: insert : (∀ (a) (a a -> Order) a (Heap a) -> (Heap a)))
(define (insert f x ts)
  (insTree f (Node 0 x '()) ts))
