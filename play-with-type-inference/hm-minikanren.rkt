#lang racket

(require minikanren)
(require "mk-helper.rkt")

(define (infer expr)
  (let ([res (run* (type)
               (!-o '() expr type))])
    (if (null? res)
        res
        (car res))))

(define (gen-exp n type)
  (run `,n (expr)
    (!-o '() expr type)))

(define (!-o gamma expr type)
  (conde
    [(symbolo expr)
     (lookupo gamma expr type)]
    [(numbero expr)
     (== 'int type)]
    [(booleano expr)
     (== 'bool type)]
    [(fresh (x e T1 T2)
       (== `(lambda (,x) ,e) expr)
       (== `(,T1 -> ,T2) type)
       (symbolo x)
       (!-o `((,x : ,T1) . ,gamma) e T2))]
    [(fresh (f e e^ t-ignore)
       (== `(let ((,f ,e)) ,e^) expr)
       (symbolo f)
       (!-o `((,f poly ,e ,gamma) . ,gamma) e^ type)
       (!-o gamma e t-ignore))]
    [(fresh (e1 e2 T)
       (== `(,e1 ,e2) expr)
       (!-o gamma e1 `(,T -> ,type))
       (!-o gamma e2 T))]
    [(fresh (op e1 e2)
       (== `(,op ,e1 ,e2) expr)
       (membero op '(+ - * /))
       (== 'int type)
       (!-o gamma e1 'int)
       (!-o gamma e2 'int))]
    [(fresh (e1 e2 T1 T2)
       (== `(cons ,e1 ,e2) expr)
       (== `(pair ,T1 ,T2) type)
       (!-o gamma e1 T1)
       (!-o gamma e2 T2))]
    [(fresh (e1 e2 e3)
       (== `(if ,e1 ,e2 ,e3) expr)
       (!-o gamma e1 'bool)
       (!-o gamma e2 type)
       (!-o gamma e3 type))]))

(define (lookupo gamma x t)
  (fresh ()
    (symbolo x)
    (conde
      [(fresh (e gamma^ _)
         (== `((,x poly ,e ,gamma^) . ,_) gamma)
         (!-o gamma^ e t))]
      [(fresh (_)
         (== `((,x : ,t) . ,_) gamma))]
      [(fresh (y _ gamma^)
         (== `((,y . ,_) . ,gamma^) gamma)
         (=/= x y)
         (symbolo y)
         (lookupo gamma^ x t))])))
