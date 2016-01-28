#lang dracula
;; Load the J-Bob language:
(include-book "j-bob-lang" :dir :teachpacks)
;; Load J-Bob, our little proof assistant:
(include-book "j-bob" :dir :teachpacks)

;; The first argument to J-Bob/step is a list of representations
;; of definitions, in this case (prelude) representing J-Bob's axioms
;; and initial functions.
;; The second argument represents an expression to rewrite.
;; The third argument is a list of steps, processed first to last,
;; to rewrite the expression.

(J-Bob/step (prelude)
  '(car (cons 'ham '(cheese)))
  '()) ;; here the list of steps is empty.

;; What are representations of expressions and definitions?
;; We represent expressions and definitions as quoted values.
;; For instance, to represent the expression x, we write 'x,
;; which is short for (quote x). Our expressions include variable
;; names, e.g 'x; quoted values e.g. ''eggs; if expressions, e.g.
;; '(if x y z), described in chapter 2; and function applications e.g.
;; '(cons 'eggs x). Definitions may be theorems or functions, such as
;; '(dethm truth () 't) and '(defun id (x) x), described in chapter 3.


;; And what are steps to rewrite an expression?
;; We shall see. What value is this expression equal to?
(J-Bob/step (prelude)
  '(car (cons 'ham '(cheese)))
  '((() (car/cons 'ham '(cheese)))))
;; ''ham
;; which represents the result of the rewrite in frame 8 of
;; chapter 1. But why is there an empty list in the first step?

;; That list is the path to the focus.
;; The path is a list of directions from the current expression
;; to the subexpression representing the focus in the pending rewrite.
;; Here, since the focus is the entire expression
;; (otherwise put, the context is empty), the path is empty as well.

(defun example1 ()
  (J-Bob/step (prelude)
    '(car (cons 'ham '(eggs)))
    '(((1) (cons 'ham '(eggs)))
      (() (car '(ham eggs))))))

(defun example1-2 ()
  (J-Bob/step (prelude)
    '(car (cons 'ham '(eggs)))
    '((() (car/cons 'ham '(eggs))))))

(defun example2 ()
  (J-Bob/step (prelude)
    '(atom '())
    '((() (atom '())))))

(defun example3 ()
  (J-Bob/step (prelude)
    '(atom (cons 'ham '(eggs)))
    '(((1) (cons 'ham '(eggs)))
      (() (atom '(ham eggs))))))

(defun example4 ()
  (J-Bob/step (prelude)
    '(atom (cons a b))
    '((() (atom/cons a b)))))

(defun example5 ()
  (J-Bob/step (prelude)
    '(equal 'flapjack (atom (cons a b)))
    '(((2) (atom/cons a b))
      (() (equal 'flapjack 'nil)))))

(defun example6 ()
  (J-Bob/step (prelude)
    '(atom (cdr (cons (car (cons p q)) '())))
    '(((1 1 1) (car/cons p q))
      ((1) (cdr/cons p '()))
      (() (atom '())))))

(defun example7 ()
  (J-Bob/step (prelude)
    '(atom (cdr (cons (car (cons p q)) '())))
    '(((1) (cdr/cons (car (cons p q)) '()))
      (() (atom '())))))

(defun example8 ()
  (J-Bob/step (prelude)
    '(car (cons (equal (cons x y) (cons x y)) '(and crumpets)))
    '(((1 1) (equal-same (cons x y)))
      (() (car/cons 't '(and crumpets))))))

(defun example9 ()
  (J-Bob/step (prelude)
    '(equal (cons x y) (cons 'bagels '(and lox)))
    '((() (equal-swap (cons x y) (cons 'bagels '(and lox)))))))

;; some exercises in chap1
(defun example10 ()
  (J-Bob/step (prelude)
    '(cons y
           (equal (car (cons (cdr x) (car y)))
                  (equal (atom x) 'nil)))
    '(((2 1) (car/cons (cdr x) (car y))))))

(defun example11 ()
  (J-Bob/step (prelude)
    '(cons y
           (equal (car (cons (cdr x) (car y)))
                  (equal (atom x) 'nil)))
    '(((2 1) (car/cons (car (cons (cdr x) (car y))) '(oats)))
      ((2 2 2) (atom/cons (atom (cdr (cons a b))) (equal (cons a b) c)))
      ((2 2 2 1 1 1) (cdr/cons a b))
      ((2 2 2 1 2) (equal-swap (cons a b) c)))))

(J-Bob/step (prelude)
  '(if a c c)
  '())

(J-Bob/step (prelude)
  '(if a c c)
  '((() (if-same a c))))

(J-Bob/step (prelude)
  '(if a c c)
  '((() (if-same a c))
    (()
     (if-same
      (if (equal a 't)
          (if (equal 'nil 'nil) a b)
          (equal 'or
                 (cons 'black '(coffee))))
      c))
    ((Q E 2) (cons 'black '(coffee)))
    ((Q A Q) (equal-same 'nil))
    ((Q A) (if-true a b))))
