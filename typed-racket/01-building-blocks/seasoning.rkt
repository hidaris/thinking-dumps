#lang typed/racket

(define-type seasoning
  (U Salt Pepper))

(struct Salt () #:transparent)
(struct Pepper () #:transparent)
