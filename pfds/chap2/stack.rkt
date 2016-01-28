#lang typed/racket

(define-type (Stack a) (Listof a))

(define empty '())

(: isEmpty : (∀ (a) (Stack a) -> Boolean))
(define (isEmpty s)
  (null? s))

(: Cons : (∀ (a) a (Stack a) -> (Stack a)))
(define (Cons x s) (cons x s))

(: head : (∀ (a) (Stack a) -> a))
(define (head s) (car s))

(: tail : (∀ (a) (Stack a) -> (Stack a)))
(define (tail s) (cdr s))

(: ++ : (∀ (a) (Stack a) (Stack a) -> (Stack a)))
(define (++ xs ys)
  (if (isEmpty xs)
      ys
      (Cons (head xs)
            (++ (tail xs) ys))))

; rewrite this function using pattern matching:
(: ++m : (∀ (a) (Stack a) (Stack a) -> (Stack a)))
(define (++m xs ys)
  (match xs
    ['() ys]
    [`(,x . ,rest)
     (Cons x (++m rest ys))]))

(struct subscript exn:fail:user () #:transparent)
(define SUBSCRIPT
  (subscript
   "can not apply update on empty"
   (current-continuation-marks)))

(: update : (∀ (a) (Stack a) Integer a -> (Stack a)))
(define (update xs i y)
  (match* (xs i)
    [('() i) (raise SUBSCRIPT)]
    [(`(,x . ,rest) 0) (Cons y rest)]
    [(`(,x . ,rest) i) (Cons x (update rest (- i 1) y))]))

(: suffixes : (∀ (a) (Stack a) -> (Stack (Stack a))))
(define (suffixes xs)
  (match xs
    ['() (Cons '() '())]
    [`(,x . ,rest)
     (Cons xs
           (suffixes rest))]))
