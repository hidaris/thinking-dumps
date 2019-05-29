#lang racket/base

;; Load the J-Bob language:
(require "./j-bob/j-bob-lang.rkt")
;; Load J-Bob, our little proof assistant:
(require "./j-bob/j-bob.rkt")
;;; What's in a name?

(defun pair (x y)
  (cons x (cons y '())))

(defun first-of (x)
  (car x))

(defun second-of (x)
  (car (cdr x)))

(dethm first-of-pair (a b) ; a claim of therom
  (equal (first-of (pair a b)) a))

;;; The Law of Defun (initial)
;;; Given the non-recursive function
;;; (defun name (x1 ... xn) body),
;;; (name e1 ... en) = body where x1 is e1, ..., xn is en

(dethm second-of-pair (a b)
  (equal (second-of (pair a b)) b))

(defun in-pair? (xs)
  (if (equal (first-of xs) '?)
      't
      (equal (second-of xs) '?)))

(dethm in-first-of-pair (b)
  (equal (in-pair? (pair '? b)) 't))

(dethm in-second-of-pair (a)
  (equal (in-pair? (pair a '?)) 't))

;;; Insight: Skip Irrelevant Expressions
;;; Rewriting a claim to 't does not have to go in
;;; any particular order. Some parts of the expression
;;; might be skipped entirely. For example, if-same can
;;; simplify many if expressions to 't regardless of the
;;; if question.
