#lang racket

(require "parenthec.rkt")

(define-union expr
  (const expr)
  (var n)
  (if test conseq alt)
  (mult exp1 exp2)
  (sub1 exp)
  (zero exp)
  (letcc body)
  (throw k-exp v-exp)
  (let exp body)
  (lambda body)
  (app rator rand))

(define value-of-cps
  (lambda (exp env k)
    (union-case exp expr
      [(const expr) (app-k k expr)]
      [(mult exp1 exp2)
       (value-of-cps exp1 env (*-mult-outer-k exp2 env k))]
      [(sub1 exp) (value-of-cps exp env (*-sub1-k k))]
      [(zero exp) (value-of-cps exp env (*-zero-k k))]
      [(if test conseq alt)
       (value-of-cps test env (*-if-k conseq alt env k))]
      [(let e body)
       (value-of-cps e env (*-let-k body env k))]
      ;; we don't need let/cc to grap cont, because we are cps!
      [(letcc body) (value-of-cps body (extend-env k env) k)]
      [(throw k-exp v-exp)
       (value-of-cps k-exp env (*-throw-k v-exp env))]
      [(var y) (apply-env env y k)]
      [(lambda body)
       (app-k k (clos_closure body env))]
      [(app rator rand)
       (value-of-cps rator env (*-rator-k rand env k))])))

(define empty-env
  (lambda ()
    `(empty-env)))

(define extend-env
  (lambda (a env)
    `(ext-env ,a ,env)))

;; (define closure
;;   (lambda (body env)
;;     `(closure ,body ,env)))
(define-union clos
  (closure body env))

(define apply-closure
  (λ (p a k)
    (union-case p clos
      [(closure body env)
       (value-of-cps body (extend-env a env) k)])))

(define apply-env
  (lambda (env n k)
    (match env
      [`(empty-env) (error 'value-of "unbound identifier")]
      [`(ext-env ,a ,env)
       (if (zero? n)
           (app-k k a)
           (apply-env env (sub1 n) k))])))

(define empty-k
  (lambda ()
    `(empty-k)))

(define *-mult-inner-k
  (lambda (v^ k)
    `(*-mult-inner-k ,v^ ,k)))

(define *-mult-outer-k
  (lambda (x2 env k)
    `(*-mult-outer-k ,x2 ,env ,k)))

(define *-sub1-k
  (lambda (k)
    `(*-sub1-k ,k)))

(define *-zero-k
  (lambda (k)
    `(*-zero-k ,k)))

(define *-if-k
  (lambda (conseq alt env k)
    `(*-if-k ,conseq ,alt ,env ,k)))

(define *-let-k
  (lambda (body env k)
    `(*-let-k ,body ,env ,k)))

;; discard outer k
(define *-throw-k
  (lambda (v-exp env)
    `(*-throw-k ,v-exp ,env)))

(define *-rand-k
  (lambda (r k)
    `(*-rand-k ,r ,k)))

(define *-rator-k
  (lambda (rand env k)
    `(*-rator-k ,rand ,env ,k)))

(define app-k
  (lambda (k v)
    (match k
      [`(empty-k) v]
      [ `(*-mult-inner-k ,v^ ,k)
        (app-k k (* v^ v))]
      [ `(*-mult-outer-k ,x2 ,env ,k)
        (value-of-cps x2 env
                    (*-mult-inner-k v k))]
      [`(*-sub1-k ,k)
       (app-k k (sub1 v))]
      [`(*-zero-k ,k)
       (app-k k (zero? v))]
      [`(*-if-k ,conseq ,alt ,env ,k)
       (if v
          (value-of-cps conseq env k)
          (value-of-cps alt env k))]
      [`(*-let-k ,body ,env ,k)
       (value-of-cps body (extend-env v env) k)]
      [`(*-throw-k ,v-exp ,env)
       (value-of-cps v-exp env v)]
      [`(*-rand-k ,r ,k)
       (apply-closure r v k)]
      [`(*-rator-k ,rand ,env ,k)
       (value-of-cps rand env
                    (*-rand-k v k))])))

;; (let ((f (lambda (f)
;;            (lambda (n)
;;              (if (zero? n)
;;                  1
;;                  (* n ((f f) (sub1 n))))))))
;;   (* (catch k ((f f) (throw k ((f f) 4)))) 5))

(define main
  (lambda ()
    (value-of-cps
     (expr_let
      (expr_lambda
       (expr_lambda
        (expr_if
         (expr_zero (expr_var 0))
         (expr_const 1)
         (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
      (expr_mult
       (expr_letcc
        (expr_app
         (expr_app (expr_var 1) (expr_var 1))
         (expr_throw (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
       (expr_const 5)))
     (empty-env)
     (empty-k))))
