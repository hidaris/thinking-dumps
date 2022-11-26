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
       (value-of-cps exp1 env (kt_*-mult-outer-k exp2 env k))]
      [(sub1 exp) (value-of-cps exp env (kt_*-sub1-k k))]
      [(zero exp) (value-of-cps exp env (kt_*-zero-k k))]
      [(if test conseq alt)
       (value-of-cps test env (kt_*-if-k conseq alt env k))]
      [(let e body)
       (value-of-cps e env (kt_*-let-k body env k))]
      ;; we don't need let/cc to grap cont, because we are cps!
      [(letcc body) (value-of-cps body (envr_extend-env k env) k)]
      [(throw k-exp v-exp)
       (value-of-cps k-exp env (kt_*-throw-k v-exp env))]
      [(var y) (apply-env env y k)]
      [(lambda body)
       (app-k k (clos_closure body env))]
      [(app rator rand)
       (value-of-cps rator env (kt_*-rator-k rand env k))])))


(define-union envr
  (empty-env)
  (extend-env a env))

(define-union clos
  (closure body env))

(define apply-closure
  (λ (p a k)
    (union-case p clos
      [(closure body env)
       (value-of-cps body (envr_extend-env a env) k)])))

(define apply-env
  (lambda (env n k)
    (union-case env envr
      [(empty-env) (error 'value-of "unbound identifier")]
      [(extend-env a env)
       (if (zero? n)
           (app-k k a)
           (apply-env env (sub1 n) k))])))

(define-union kt
  (empty-k)
  (*-mult-inner-k v^ k)
  (*-mult-outer-k x2 env k)
  (*-sub1-k k)
  (*-zero-k k)
  (*-if-k conseq alt env k)
  (*-let-k body env k)
  ;; discard outer k
  (*-throw-k v-exp env)
  (*-rand-k r k)
  (*-rator-k rand env k))


(define app-k
  (lambda (k^ v)
    (union-case k^ kt
      [(empty-k) v]
      [(*-mult-inner-k v^ k)
        (app-k k (* v^ v))]
      [ (*-mult-outer-k x2 env k)
        (value-of-cps x2 env
                    (kt_*-mult-inner-k v k))]
      [(*-sub1-k k)
       (app-k k (sub1 v))]
      [(*-zero-k k)
       (app-k k (zero? v))]
      [(*-if-k conseq alt env k)
       (if v
          (value-of-cps conseq env k)
          (value-of-cps alt env k))]
      [(*-let-k body env k)
       (value-of-cps body (envr_extend-env v env) k)]
      [(*-throw-k v-exp env)
       (value-of-cps v-exp env v)]
      [(*-rand-k r k)
       (apply-closure r v k)]
      [(*-rator-k rand env k)
       (value-of-cps rand env
                    (kt_*-rand-k v k))])))

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
     (envr_empty-env)
     (kt_empty-k))))