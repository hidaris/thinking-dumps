#lang racket/base
(require minikanren)
;; a relational simple typed lambda calculus inferencer
;; extend with tuple type and list type

;; macro for test
(define-syntax test
  (syntax-rules ()
    [(_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ([expected expected-result]
              [produced tested-expression])
         (or (equal? expected produced)
             (printf "Failed: ~a~%Expected: ~a~%Computed: ~a~%"
                     'tested-expression expected produced))))]))

;; infer the type of expression
(define (infer expr)
  (let ([res (run* (type)
               (typeofo '() expr type))])
    (if (null? res)
        res
        (car res))))

;; give a number n and type t
;; we can generate n expressions
;; which type are t, called "run backwards"
(define (gen-exp n type)
  (run `,n (expr)
    (typeofo '() expr type)))

(define (typeofo Γ e τ)
  (conde
    [(symbolo e) (lookupo Γ e τ)]
    [(numbero e) (== 'num τ)]
    [(fresh (e1 e2)
       (== `(+ ,e1 ,e2) e)
       (typeofo Γ e1 'num)
       (typeofo Γ e2 'num)
       (== 'num τ))]
    ;; const types: +, nil, cons
    [(fresh (_)
       (== '+ e)
       (== '(num -> num -> num) τ))]
    [(fresh (_)
       (== 'nil e)
       (== 'Null τ))]
    [(fresh (_)
       (== 'cons e)
       (== '(case-> (num -> (list num) -> (list num))
                    (num -> num -> (num * num)))
           τ))]
    ;; const types over
    [(fresh (x e^ τ1 τ2)
       (== `(λ (,x ,τ1) ,e^) e)
       (== `(,τ1 -> ,τ2) τ)
       (symbolo x)
       (typeofo `((,x : ,τ1) . ,Γ) e^ τ2))]
    [(fresh (e1 e2 T)
       (== `(,e1 ,e2) e)
       (typeofo Γ e1 `(,T -> ,τ))
       (typeofo Γ e2 T))]
    [(fresh (e1 e2 e3)
       (== `(if0 ,e1 ,e2 ,e3) e)
       (typeofo Γ e1 'num)
       (typeofo Γ e2 τ)
       (typeofo Γ e3 τ))]
    ;; cons(num, num)
    [(fresh (e1 e2 τ1 τ2)
       (== `(cons ,e1 ,e2) e)
       (== `(,τ1 * ,τ2) τ)
       (=/= e2 'nil)
       (numbero e2)
       (typeofo Γ e1 τ1)
       (typeofo Γ e2 τ2))]
    ;; cons(num, cons(num, cons(num num)))
    [(fresh (e1 e2 τ1 τ2)
       (== `(cons ,e1 ,e2) e)
       (== `(,τ1 * ,τ2) τ)
       (=/= e2 'nil)
       (fresh (e3 e4)
         (== e2 `(cons ,e3 ,e4))
         (=/= e4 'nil))
       (typeofo Γ e1 τ1)
       (typeofo Γ e2 τ2))]
    ;; cons(1, cons(2, nil)) => '(1 2)
    [(fresh (e1 e2 τ1 τ2)
       (== `(cons ,e1 ,e2) e)
       (== `(list ,τ1) τ)
       (=/= e2 'nil)
       (fresh (e3 e4)
         (== e2 `(cons ,e3 ,e4))
         (== e4 'nil))
       (typeofo Γ e1 τ1)
       (typeofo Γ e2 τ2)
       (== τ1 τ2))]
    ;; cons(1, nil)
    [(fresh (e1 e2 τ1 τ2)
       (== `(cons ,e1 ,e2) e)
       (== `(list ,τ1) τ)
       ;; (== e2 'nil)
       (typeofo Γ e1 τ1)
       (typeofo Γ e2 τ2))]))

(define (lookupo Γ x t)
  (fresh ()
    (symbolo x)
    (conde
      [(fresh (_)
         (== `((,x : ,t) . ,_) Γ))]
      [(fresh (y _ Γ^)
         (symbolo y)
         (== `((,y . ,_) . ,Γ^) Γ)
         (=/= x y)
         (lookupo Γ^ x t))])))

;; test infer
(test "typeof number"
      (infer 1)
      'num)

;; test const type
(test "typeof nil"
      (infer 'nil)
      'Null)

(test "typeof +"
      (infer '+)
      '(num -> num -> num))

(test "typeof cons"
      (infer 'cons)
      '(case-> (num -> (list num) -> (list num))
               (num -> num -> (num * num))))
;; test const type over

(test "typeof + function"
      (infer '(+ 2 3))
      'num)

(test "typeof cons0"
      (infer '(cons 2 3))
      '(num * num))

(test "typeof cons1"
      (infer '(cons 2 nil))
      '(list num))

(test "typeof cons2"
      (infer '(cons 1 (cons 2 3)))
      '(num * (num * num)))

(test "typeof cons3"
      (infer '(cons 1 (cons 2 nil)))
      '(list num))

(test "typeof if0"
      (infer '(if0 0 2 1))
      'num)

(test "typeof λ"
      (infer '(λ (x num) x))
      '(num -> num))

(test "typeof application"
      (infer '((λ (x num) x) 2))
      'num)

;; test run backwards
(test "generate 2 num exp"
      (gen-exp 2 'num)
      '((_.0 (num _.0))
        ((+ _.0 _.1) (num _.0 _.1))))

(test "generate 2 (num -> num -> num) exp"
      (gen-exp 2 '(num -> num -> num))
      '(+
        ((if0 _.0 + +) (num _.0))))

(test "generate 2 Null exp"
      (gen-exp 2 'Null)
      '(nil
        ((if0 _.0 nil nil) (num _.0))))

(test "generate 2 cons type exp"
      (gen-exp 2 '(case-> (num -> (list num) -> (list num))
                          (num -> num -> (num * num))))
      '(cons
        ((if0 _.0 cons cons) (num _.0))))

(test "generate 2 (num * num) exp"
      (gen-exp 2 '(num * num))
      '(((cons _.0 _.1) (num _.0 _.1))
        ((cons (+ _.0 _.1) _.2) (num _.0 _.1 _.2))))

(test "generate 2 (list num) exp"
      (gen-exp 2 '(list num))
      '(((cons _.0 _.1) (num _.0 _.1))
        ((cons _.0 +) (num _.0))))
