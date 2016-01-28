#lang typed/racket

(define-type open_faced_sandwich
  (U Bread Slice))

(struct Bread ([v : Any]) #:transparent)
(struct Slice ([v : open_faced_sandwich]) #:transparent)
