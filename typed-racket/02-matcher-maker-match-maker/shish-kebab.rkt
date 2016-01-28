#lang typed/racket

(define-type shish_kebab
  (U Skewer
     Onion
     Lamb
     Tomato))

(struct Skewer () #:transparent)
(struct Onion ([v : shish_kebab])
  #:transparent)
(struct Lamb ([v : shish_kebab])
  #:transparent)
(struct Tomato ([v : shish_kebab])
  #:transparent)

(: only_onions (-> shish_kebab Boolean))
(define (only_onions sk)
  (match sk
    [(Skewer) true]
    [(Onion x) (only_onions x)]
    [(Lamb x) false]
    [(Tomato x) false]))

(: is_vegetarian (-> shish_kebab Boolean))
(define (is_vegetarian iv)
  (match iv
    [(Skewer) true]
    [(Onion x) (is_vegetarian x)]
    [(Lamb x) false]
    [(Tomato x) (is_vegetarian x)]))
