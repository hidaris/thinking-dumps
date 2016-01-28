#lang typed/racket

(define-type (Tree a) (U E (T a)))
(struct E () #:transparent)
(struct (a) T ([l : (Tree a)]
               [e : a]
               [r : (Tree a)])
  #:transparent)

(define-type Order (U LT EQ GT))
(struct LT () #:transparent)
(struct EQ () #:transparent)
(struct GT () #:transparent)

(define empty (E))

(: insert : (∀ (a)
               (a a -> Order) a (Tree a) -> (Tree a)))
(define (insert f x t)
  (match t
    [(E) (T (E) x (E))]
    [(T l e r)
     (match (f x e)
       [(LT) (T (insert f x l) e r)]
       [(EQ) t]
       [(GT) (T l e (insert f x r))])]))

(: member : (∀ (a)
               (a a -> Order) a (Tree a) -> Boolean))
(define (member f x t)
  (match t
    [(E) false]
    [(T l e r)
     (match (f x e)
       [(LT) (member f x l)]
       [(EQ) true]
       [(GT) (member f x r)])]))
