#lang typed/racket

(define-type Exp (U Val Add))
(define-predicate Exp? Exp)
(struct Val ([v : Integer]) #:transparent)
(struct Add ([e1 : Exp] [e2 : Exp]) #:transparent)

(: eval : Exp -> Integer)
(define (eval e)
  (match e
    [(Val n) n]
    [(Add e1 e2) (+ (eval e1)
                    (eval e2))]))

(define-type Exp+Mul (U Exp Mul))
(struct Mul ([e1 : Exp+Mul] [e2 : Exp+Mul]) #:transparent)

(: eval+mul : Exp+Mul -> Integer)
(define (eval+mul e)
  (cond
    [(Exp? e) (eval e)]
    [else
     (match e
       [(Mul e1 e2) (- (eval+mul e1)
                       (eval+mul e2))])]))
