#lang typed/racket

(struct Foo ([a : String] [b : Integer]) #:transparent)
(struct Bar ([a : String] [b : Integer]) #:transparent)

;; (: foo Bar)
;; (define foo (Foo "foo" 1))
;; aha, TR is nominal type.

(define-type ftype (-> String Number))
