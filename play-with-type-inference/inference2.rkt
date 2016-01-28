#lang racket
(require "mk-symbol.rkt")


; Assuming that term is in the condition position
; of an if-statement, what propositions do we learn about
; the then and else branches?
(define (infer-props term then-prop else-prop)
  (conde
    [(== #f term)
     (== 'ff then-prop)
     (== 'tt else-prop)]
    [(conde
       [(== #t term)]
       [(numbero term)])
     (== 'tt then-prop)
     (== 'ff else-prop)]
    [(symbolo term)
     (== `(,term (not (val #f))) then-prop)
     (== `(,term (val #f)) else-prop)]))

; Look up var in the proposition environment and find
; type information res.
(define (lookupo var env res)
  (conde
    [(fresh (d)
       (== `((,var ,res) . ,d) env))]
    [(fresh (aa ad d)
       (=/= var aa)
       (== `((,aa ,ad) . ,d) env)
       (lookupo var d res))]))

; Prove that var is compatible with type given the proposition environment
(define (proveo prop-env var type)
  (conde
    [(fresh (t1)
       (lookupo var prop-env t1)
       (subtypeo t1 type))]))

(define (booleano b)
  (conde
    [(== #t b)]
    [(== #f b)]))

; Succeed if child-type is a subtype of parent-type,
; like (var #f) is a subtype of 'bool.
(define (subtypeo child-type parent-type)
  (conde
    [(== child-type parent-type)]
    [(fresh (b)
       (== `(val ,b) child-type)
       (conde
         [(booleano b)
          (== 'bool parent-type)]
         [(numbero b)
          (== 'num parent-type)]))]
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

; Given term and the proposition environment, infer the most
; specific type for the term. If you want to check compatiblity
; of the term's type with a given type, use the subtype relation
; with the type argument of this relation.
(define (infer term prop-env type)
  (conde
    [(== #f term)
     (== '(val #f) type)]
    [(== #t term)
     (== '(val #t) type)]
    [(numbero term)
     (== `(val ,term) type)]
    [(fresh (condition then else then-prop
             else-prop cond-type t1 t2)
       (== `(if ,condition ,then ,else) term)
       (infer condition prop-env cond-type)
       (subtypeo cond-type 'bool)
       (infer-props condition then-prop else-prop)
       (infer then `(,then-prop . ,prop-env) t1)
       (infer else `(,else-prop . ,prop-env) t2)
       (uniono t1 t2 type))]
    [(fresh (arg argtype body prop-env^ res-type)
       (== `(lambda (,arg : ,argtype) ,body) term)
       (== `((,arg ,argtype) . ,prop-env) prop-env^)
       (infer body prop-env^ res-type)
       (subtypeo `(,argtype -> ,res-type) type))]
    [(fresh ()
       (symbolo term)
       (proveo prop-env term type))]
    [(fresh (expr expr-type)
       (== `(inc ,expr) term)
       (infer expr prop-env expr-type)
       (subtypeo expr-type 'num)
       (== 'num type))]))
