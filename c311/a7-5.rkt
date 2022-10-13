#lang racket


;; $ stand for $tream
(define-syntax cons$
  (syntax-rules ()
    ((cons$ x y) (cons x (delay y)))))


(define car$ car)


(define cdr$
  (lambda ($) (force (cdr $))))


(define take$
  (lambda (n $)
    (cond
      [(zero? n) '()]
      [else (cons (car$ $)
                  (let ((n- (sub1 n)))
                    (cond
                      [(zero? n-) '()]
                      [else (take$ n- (cdr$ $))])))])))


(define trib$
  (letrec ([p (lambda (a)
                (lambda (b)
                  (lambda (c)
                    (cons$ a (((p b) c) (+ a b c))))))])
    (((p 0) 1) 1)))
