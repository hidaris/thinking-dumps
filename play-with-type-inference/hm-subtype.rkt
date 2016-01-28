#lang racket

(require minikanren)
(require "mk-helper.rkt")

(define (subtypeo child-type parent-type)
  (conde
    [(== child-type parent-type)]
    [(fresh (b)
       (== `(intc ,b) child-type)
       (conde
         [(numbero b)
          (== 'int parent-type)]))]
    [(fresh (b)
       (== `(boolc ,b) child-type)
       (conde
         [(booleano b)
          (== 'bool parent-type)]))]
    [(fresh (t1 t2)
       (== `(U ,t1 ,t2) child-type)
       (subtypeo t1 parent-type)
       (subtypeo t2 parent-type))]))

; Union type constructor. We might need to make this
; smarter in the future.
(define (uniono t1 t2 union-type)
  (conde
    [(== t1 t2)
     (== t1 union-type)]
    [(=/= t1 t2)
     (== `(U ,t1 ,t2) union-type)]))

(define (infer expr)
  (let ([res (run* (type)
               (!-o '() expr type))])
    (if (null? res)
        res
        (car res))))

(define (gen-exp n type)
  (run `,n (expr)
    (!-o '() expr type)))

(define (!-o Γ expr type)
  (conde
    [(symbolo expr)
     (lookupo Γ expr type)]
    [(numbero expr)
     (== `(intc ,expr) type)]
    [(booleano expr)
     (== `(boolc ,expr) type)]
    [(fresh (x e T1 T2)
       (== `(lambda (,x) ,e) expr)
       (== `(,T1 -> ,T2) type)
       (symbolo x)
       (!-o `((,x : ,T1) . ,Γ) e T2))]
    [(fresh (f e e^ t-ignore)
       (== `(let ((,f ,e)) ,e^) expr)
       (symbolo f)
       (!-o `((,f poly ,e ,Γ) . ,Γ) e^ type)
       (!-o Γ e t-ignore))]
    [(fresh (e1 e2 T)
       (== `(,e1 ,e2) expr)
       (!-o Γ e1 `(,T -> ,type))
       (!-o Γ e2 T))]
    [(fresh (op e1 e2 e1-t e2-t)
       (== `(,op ,e1 ,e2) expr)
       (membero op '(+ - * /))
       (== 'int type)
       (!-o Γ e1 e1-t)
       (subtypeo e1-t 'int)
       (!-o Γ e2 e2-t)
       (subtypeo e2-t 'int))]
    [(fresh (e1 e2 T1 T2)
       (== `(cons ,e1 ,e2) expr)
       (== `(pair ,T1 ,T2) type)
       (!-o Γ e1 T1)
       (!-o Γ e2 T2))]
    [(fresh (e1 e2 e3 cond-type)
       (== `(if ,e1 ,e2 ,e3) expr)
       (!-o Γ e1 cond-type)
       (subtypeo cond-type 'bool)
       (!-o Γ e2 type)
       (!-o Γ e3 type))]))

(define (lookupo Γ x t)
  (fresh ()
    (symbolo x)
    (conde
      [(fresh (e Γ^ _)
         (== `((,x poly ,e ,Γ^) . ,_) Γ)
         (!-o Γ^ e t))]
      [(fresh (_)
         (== `((,x : ,t) . ,_) Γ))]
      [(fresh (y _ Γ^)
         (== `((,y . ,_) . ,Γ^) Γ)
         (=/= x y)
         (symbolo y)
         (lookupo Γ^ x t))])))
