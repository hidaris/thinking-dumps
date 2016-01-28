#lang typed/racket

;; leftist heaps are heap-ordered binary trees that
;; satisfy the leftist property: the rank of any left child is at
;; least as large as the rank of its right sibling.
;; the rank of a node is defined to be the length of its right spine
;; (i.e., the rightmost path from the node in question to an empty node).
;; a simple consequence of the leftist property is that the right spine of
;; any node is always the shortest path to an empty node.

(define-type (Heap a) (U Empty (Tree a)))
(define-type Int Integer)
(define-type Bool Boolean)

(struct Empty () #:transparent)

(struct (a) Tree
  ([rank  : Int]
   [elem  : a]
   [left  : (Heap a)]
   [right : (Heap a)])
  #:transparent)

(define empty (Empty))

(: empty? : (∀ (a)
                (Heap a) -> Bool))
(define (empty? heap)
  (match heap
    [(Empty) true]
    [_ false]))

;; O(logn)
(: merge : (∀ (a)
              (a a -> Order)
              (Heap a)
              (Heap a)
              -> (Heap a)))
(define (merge f h1 h2)
  (match* (h1 h2)
    [(_ (Empty)) h1]
    [((Empty) _) h2]
    [((Tree _ x a1 b1) (Tree _ y a2 b2))
     (match (f x y)
       [(LT) (makeT x a1 (merge f b1 h2))]
       [else (makeT y a2 (merge f h1 b2))])]))

;; since I'm wonder about how to use
;; sml's structure idiom in TR
;; use a ... way
(define-type Order (U LT EQ GT))
(struct LT () #:transparent)
(struct EQ () #:transparent)
(struct GT () #:transparent)

(: leq : (∀ (a) (a a -> Bool)))
(define (leq e1 e2)
  (leq e1 e2))

;;; get-rank
(: rank : (∀ (a) (Heap a) -> Int))
(define (rank heap)
  (match heap
    [(Empty) 0]
    [(Tree r _ _ _) r]))

;;; makeT is a helper that calculates the rank of
;;; a tree node and swaps its children if necessary.
(: makeT : (∀ (a) a (Heap a) (Heap a) -> (Heap a)))
(define (makeT x a b)
  (let ([ra (rank a)]
        [rb (rank b)])
    (if (>= ra rb)
        (Tree (add1 rb) x a b)
        (Tree (add1 ra) x b a))))

(: insert : (∀ (a)
               (a a -> Order)
               a
               (Heap a)
               -> (Heap a)))
(define (insert f x h)
  (merge f (Tree 1 x empty empty) h))

(struct Emp exn:fail:user () #:transparent)
(define EMPTY
  (Emp
   "can not apply on empty"
   (current-continuation-marks)))

(: findMin : (∀ (a) (Heap a) -> a))
(define (findMin h)
  (match h
    [(Empty) (raise EMPTY)]
    [(Tree _ x a b) x]))

(: deleteMin : (∀ (a)
                  (a a -> Order)
                  (Heap a)
                  -> (Heap a)))
(define (deleteMin f h)
  (match h
    [(Empty) (raise EMPTY)]
    [(Tree _ x a b) (merge f a b)]))
