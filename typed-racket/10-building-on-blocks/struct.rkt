#lang typed/racket

(define-type number
  (U Zero
     One-more-than))

(struct Zero ()
  #:transparent)
(struct One-more-than ([v : number])
  #:transparent)

(: succ : number -> number)
(define (succ n)
  (One-more-than n))

(struct Too-small exn:fail:user ())

(: pred : number -> number)
(define (pred n)
  (match n
    [(Zero) (raise
             (Too-small
              "Too-small"
              (current-continuation-marks)))]
    [(One-more-than m) m]))

(define (is-zero n)
  (match n
    [(Zero) true]
    [_ false]))
