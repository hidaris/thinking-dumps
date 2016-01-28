#lang typed/racket

(define-type shish
  (U Bottom
     Onion
     Lamb
     Tomato))

(struct Bottom ([v : Any]) #:transparent)
;; (struct (a) Bottom ([v : a]))
(struct Onion ([v : shish]) #:transparent)
(struct Lamb ([v : shish]) #:transparent)
(struct Tomato ([v : shish]) #:transparent)

(define-type rod
  (U Dagger
     Fork
     Sword))

(struct Dagger () #:transparent)
(struct Fork () #:transparent)
(struct Sword () #:transparent)

(define-type plate
  (U Gold_plate
     Siver_plate
     Brass_plate))

(struct Gold_plate () #:transparent)
(struct Siver_plate () #:transparent)
(struct Brass_plate () #:transparent)

(: is_veggie (-> shish Boolean))
(define (is_veggie sh)
  (match sh
    [(Bottom x) true]
    [(Onion x) (is_veggie x)]
    [(Lamb x) false]
    [(Tomato x) (is_veggie x)]))

(: what_bottom (-> shish Any))
(define (what_bottom wb)
  (match wb
    [(Bottom x) x]
    [(Onion x) (what_bottom x)]
    [(Lamb x) (what_bottom x)]
    [(Tomato x) (what_bottom x)]))
