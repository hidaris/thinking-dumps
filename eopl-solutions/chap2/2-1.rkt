#lang eopl

(define zero
  '())

(define is-zero?
  null?)

(define successor
  (lambda (n)
    (cond
      [(is-zero? n) '(1)]
      [else
       (let [(first (+ (car n) 1))
             (rest (cdr n))]
         (if (= first 16)
             (cons 0 (successor rest))
             (cons first rest)))])))

(define predecessor
  (lambda (n)
    (cond
      [(is-zero? n)
       (eopl:error 'pred-zero
                   "only accept num biger than zero.~%")]
      [(>= (car n) 16) #f]
      [(equal? n '(1)) '()]
      [(zero? (car n))
       (if (null? (cdr n))
           #f
           (cons (- 16 1)
                 (predecessor (cdr n))))]
      [else (cons
             (- (car n) 1)
             (cdr n))])))
