#lang typed/racket

(define-type (Stack a) (U NIL (Cons a)))
(struct NIL () #:transparent)
(struct (a) Cons ([v : a] [w : (Stack a)])
  #:transparent)

(define empty (NIL))

(define-struct (EMPTY exn:fail:user) ())

(: isEmpty : (∀ (a) (Stack a) -> Boolean))
(define (isEmpty s)
  (match s
    [(NIL) true]
    [_ false]))

(: cons : (∀ (a) a (Stack a) -> (Stack a)))
(define (cons x s)
  (Cons x s))

(: head : (∀ (a) (Stack a) -> a))
(define (head s)
  (match s
    [(NIL) (raise (EMPTY
                   "can not apply head on empty stack"
                   (current-continuation-marks)))]
    [(Cons x s) x]))

(: tail : (∀ (a) (Stack a) -> (Stack a)))
(define (tail s)
  (match s
    [(NIL) (raise (EMPTY
                   "can not apply tail on empty stack"
                   (current-continuation-marks)))]
    [(Cons x s) s]))

(: ++ : (∀ (a) (Stack a) (Stack a) -> (Stack a)))
(define (++ xs ys)
  (if (isEmpty xs)
      ys
      (cons (head xs)
            (++ (tail xs) ys))))

(: ++m : (∀ (a) (Stack a) (Stack a) -> (Stack a)))
(define (++m xs ys)
  (match xs
    [(NIL) ys]
    [(Cons x xss)
     (cons x (++m xss ys))]))

(struct subscript exn:fail:user () #:transparent)
(define SUBSCRIPT
  (subscript
   "can not apply update on empty"
   (current-continuation-marks)))

(: update : (∀ (a) (Stack a) Integer a -> (Stack a)))
(define (update xs i y)
  (match* (xs i)
    [((NIL) i) (raise SUBSCRIPT)]
    [((Cons x rest) 0) (Cons y rest)]
    [((Cons x rest) i) (Cons x (update rest (- i 1) y))]))

(: suffixes : (∀ (a) (Stack a) -> (Stack (Stack a))))
(define (suffixes xs)
  (match xs
    [(NIL) (Cons (NIL) (NIL))]
    [(Cons x rest)
     (Cons xs
           (suffixes rest))]))
