#lang racket/base

;; Load the J-Bob language:
(require "j-bob/j-bob-lang.rkt")
;; Load J-Bob, our little proof assistant:
(require "j-bob/j-bob.rkt")
;; Load the transcript of all proofs in the book:
(require "j-bob/little-prover.rkt")
;;; Old Games, New Rules

;;; The Axioms of Cons (initial)
(dethm atom/cons (x y)
  (equal (atom (cons x y)) 'nil))

(dethm car/cons (x y)
  (equal (car (cons x y)) x))

(dethm cdr/cons (x y)
  (equal (cdr (cons x y)) y))

;;; a theorem is an expression that is always true.
;;; when we use dethm, we also include a list of the
;;; variables used in the expression.

;;; axioms are theorems that are assumed to be true,
;;; whereas other theorems must be shown to be true.

;;; The Axioms of Equal (initial)
(dethm equal-same (x)
  (equal (equal x x) 't))

(dethm equal-swap (x y)
  (equal (equal x y) (equal y x)))

;;; for any theorem (dethm name (x1, ... xn) bodyx), the variables
;;; x1, ... xn in bodyx can be replaced with any corresponding expressions
;;; e1, ... en. The result, bodye, can be used to rewrite a focus p to
;;; become q provide bodye is either (equal p q) or (equal q p).
