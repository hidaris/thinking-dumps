#lang typed/racket

(define-type Expr (U Lit Add))
(struct Lit ([i : Integer]) #:transparent)
(struct Add ([e1 : Expr] [e2 : Expr]) #:transparent)

(: e : Expr)
(define e (Add (Lit 1) (Add (Lit 1) (Lit 1))))

(: eval : Expr -> Integer)
(define (eval e)
  (match e
    [(Lit i) i]
    [(Add e1 e2)
     (+ (eval e1) (eval e2))]))

(: view : Expr -> String)
(define (view e)
  (match e
    [(Lit i) (number->string i)]
    [(Add e1 e2)
     (string-append
      (view e1) "+" (view e2))]))
