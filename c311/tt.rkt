#lang racket

(require "mkk.rkt")

(define gamma0
  '((x . Nat) (y . Bool) (z . Nat)))

(defrel (lookupo gamma x t)
  (fresh (x^ t^ gamma^)
    (== `((,x^ . ,t^) . ,gamma^) gamma)
    (conde
     [(== x^ x) (== t^ t)]
     [(=/= x^ x) (lookupo gamma^ x t)])))

#;
(run 1 τ
  (lookupo gamma0 'x τ))

(defrel (!- gamma e t)
  (conde
   [(numbero e)
    (== t 'Nat)]
   [(conde
     [(== e #t)]
     [(== e #f)])
    (== t 'Bool)]
   [(fresh (e1 e2)
      (conde
       [(== `(* ,e1 ,e2) e)]
       [(== `(+ ,e1 ,e2) e)])
      (!- gamma e1 'Nat)
      (!- gamma e2 'Nat)
      (== t 'Nat))]
   [(fresh (e1 e2 t1 t2)
      (== `(cons ,e1 ,e2) e)
      (!- gamma e1 t1)
      (!- gamma e2 t2)
      (== t `(pairof ,t1 ,t2)))]
   [(fresh (e1 t1 t2)
      (== `(car ,e1) e)
      (!- gamma e1 `(pairof ,t1 ,t2))
      (== t t1))]
   [(fresh (e1 t1 t2)
      (== `(cdr ,e1) e)
      (!- gamma e1 `(pairof ,t1 ,t2))
      (== t t2))]
   [(fresh (e1 e2 e3)
      (== `(if ,e1 ,e2 ,e3) e)
      (!- gamma e1 'Bool)
      (!- gamma e2 t)
      (!- gamma e3 t))]
   [(fresh (e1)
      (== `(sub1 ,e1) e)
      (!- gamma e1 'Nat)
      (== t 'Nat))]
   [(fresh (e1)
      (== `(zero? ,e1) e)
      (!- gamma e1 'Nat)
      (== t 'Bool))]
   [(fresh (e1)
      (== `(not ,e1) e)
      (!- gamma e1 'Bool)
      (== t 'Bool))]
   [(fresh (e1 e2)
      (== `(and ,e1 ,e2) e)
      (!- gamma e1 'Bool)
      (!- gamma e2 'Bool)
      (== t 'Bool))]
   [(symbolo e)
    (lookupo gamma e t)]
   [(fresh (x body)
      (== `(lambda (,x) ,body) e)
      (fresh (t_in t_out)
        (== `(,t_in -> ,t_out) t)
        (!- `((,x . ,t_in) . ,gamma) body t_out)))]
   [(fresh (rator rand)
      (== `(,rator ,rand) e)
      (fresh (t_in t_out)
        (!- gamma rator `(,t_in -> ,t_out))
        (!- gamma rand t_in)
        (== t t_out)))]
   [(fresh (x body)
      (== `(fix (lambda (,x) ,body)) e)
      (!- `((,x . ,t) . ,gamma) body t))]
   [(fresh (f e1 e2 body t1)
      (== `(let ([,f ,e1]) ,body) e)
      (symbolo f)
      (!- gamma e1 t1)
      (!- `((,f . ,t1) . ,gamma) body t))]))
