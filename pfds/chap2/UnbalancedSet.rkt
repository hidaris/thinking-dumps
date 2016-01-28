#lang typed/racket

(require "Element.rkt")

(define-type Elem TT)
(define-type Tree
  (U E
     T))

(struct E ()
  #:transparent)
(struct T ([u : Tree] [v : Elem] [w : Tree])
  #:transparent)

(define-type-alias Set Tree)

(: empty : Set)
(define empty (E))

(: member : (Elem Set -> Boolean))
(define (member x s)
  (match s
    [(E) false]
    [(T a y b)
     (if (lt x y) (member x a)
         (if (lt y x)
             (member x b)
             true))]))

(: insert : (Elem Set -> Set))
(define (insert x s)
  (match s
    [(E) (T (E) x (E))]
    [(T a y b)
     (if (lt x y)
         (T (insert x a) y b)
         (if (lt y x)
             (T a y (insert x b))
             s))]))
