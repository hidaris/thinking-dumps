#lang racket/base

;; Load the J-Bob language:
(require "./j-bob/j-bob-lang.rkt")
;; Load J-Bob, our little proof assistant:
(require "./j-bob/j-bob.rkt")
;;; Even Older Games

;;; The Axioms of If (initial)
(dethm if-true (x y)
  (equal (if 't x y) x))

(dethm if-false (x y)
  (equal (if 'nil x y) y))

(dethm if-same (x y)
  (equal (if x y y) y))

;;; The Axioms of Equal (final)
(dethm equal-same (x)
  (equal (equal x x) 't))

(dethm equal-swap (x y)
  (equal (equal x y) (equal y x)))

(dethm equal-if (x y)
  (if (equal x y) (equal x y) 't))

;;; The Lay of Dethm (final)
;;; For any theorem (dethm name (x1, ... xn) bodyx), the variables
;;; x1, ... xn in bodyx can be replaced with any corresponding expressions
;;; e1, ... en. The result, bodye, can be used to rewrite a focus as follows:

;;; 1. bodye must contain the conclusion (equal p q) or (equal q p),
;;; 2. the conclusion must not be found in the question of any
;;; if or in the argument of any function application,
;;; 3. and if the conclusion can be found in an if answer
;;; (respectively else), then the focus must be found in an if
;;; answer (respectively else) with the same question.

;; a pretend axioms .
;; (dethm jabberwocky (x)
;;   (if (brillig x)
;;       (if (slithy x)
;;           (equal (mimsy x) 'borogove)
;;           (equal (mome x) 'rath))
;;       (if (uffish x)
;;           (equal (frumious x) 'bandersnatch)
;;           (equal (frabjous x) 'beamish))))

;; The Axioms of Cons (final)
(dethm atom/cons (x y)
  (equal (atom (cons x y)) 'nil))

(dethm car/cons (x y)
  (equal (car (cons x y)) x))

(dethm cdr/cons (x y)
  (equal (cdr (cons x y)) y))

(dethm cons/car+cdr (x)
  (if (atom x) 't (equal (cons (car x) (cdr x)) x)))

;;; The Axioms of If (final)
(dethm if-nest-A (x y z)
  (if x (equal (if x y z) y) 't))

(dethm if-nest-E (x y z)
  (if x 't (equal (if x y z) z)))
