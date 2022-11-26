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
      [(const expr) (let* ([k* k]
                           [v* expr])
                      (app-k k* v*))]
      [(mult exp1 exp2)
       (let* ([exp* exp1]
              [env* env]
              [k* k]
              [k* (kt_*-mult-outer-k exp2 env* k*)])
         (value-of-cps exp* env* k*))]
      [(sub1 exp) (let* ([exp* exp]
                         [env* env]
                         [k* k]
                         [k* (kt_*-sub1-k k*)])
                    (value-of-cps exp* env* k*))]
      [(zero exp) (let* ([exp* exp]
                         [env* env]
                         [k* k]
                         [k* (kt_*-zero-k k*)])
                    (value-of-cps exp* env* k*))]
      [(if test conseq alt)
       (let* ([exp* test]
              [env* env]
              [k* k]
              [k* (kt_*-if-k conseq alt env* k*)])
         (value-of-cps exp* env* k*))]
      [(let e body)
       (let* ([exp* e]
              [env* env]
              [k* k]
              [k* (kt_*-let-k body env* k*)])
         (value-of-cps exp* env* k*))]
      ;; we don't need let/cc to grap cont, because we are cps!
      [(letcc body) (let* ([exp* body]
                           [env* env]
                           [k* k]
                           [env* (envr_extend-env k* env*)])
                      (value-of-cps exp* env* k*))]
      [(throw k-exp v-exp)
       (let* ([exp* k-exp]
              [env* env]
              [k* (kt_*-throw-k v-exp env*)])
         (value-of-cps exp* env* k*))]
      [(var y) (let* ([env* env]
                      [a* y]
                      [k* k])
                 (apply-env env* a* k*))]
      [(lambda body)
       (let* ([k* k]
              [env* env]
              [v* (clos_closure body env*)])
         (app-k k* v*))]
      [(app rator rand)
       (let* ([exp* rator]
              [env* env]
              [k* k]
              [k* (kt_*-rator-k rand env* k*)])
         (value-of-cps exp* env* k*))])))


(define-union envr
  (empty-env)
  (extend-env a env))

(define-union clos
  (closure body env))

(define apply-closure
  (λ (p a k)
    (union-case p clos
      [(closure body env)
       (let* ([exp* body]
              [env* env]
              [v* a]
              [env* (envr_extend-env v* env*)]
              [k* k])
         (value-of-cps exp* env* k*))])))

(define apply-env
  (lambda (env n k)
    (union-case env envr
      [(empty-env) (error 'value-of "unbound identifier")]
      [(extend-env a env)
       (if (zero? n)
           (let* ([k* k]
                  [v* a])
             (app-k k* v*))
           (let* ([env* env]
                  [n* n]
                  [n* (sub1 n*)]
                  [k* k])
             (apply-env env* n* k*)))])))

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
      [(empty-k) (let* ([v* v])
                   v*)]
      [(*-mult-inner-k v^ k)
       (let* ([k* k]
              [v* v]
              [v* (* v^ v*)])
         (app-k k* v*))]
      [(*-mult-outer-k x2 env k)
       (let* ([exp* x2]
              [env* env]
              [k* k]
              [k* (kt_*-mult-inner-k v k*)])
         (value-of-cps exp* env* k*))]
      [(*-sub1-k k)
       (let* ([k* k]
              [v* (sub1 v)])
         (app-k k* v*))]
      [(*-zero-k k)
       (let* ([k* k]
              [v* (zero? v)])
         (app-k k* v*))]
      [(*-if-k conseq alt env k)
       (if v
           (let* ([exp* conseq]
                  [env* env]
                  [k* k])
             (value-of-cps exp* env* k*))
           (let* ([exp* alt]
                  [env* env]
                  [k* k])
             (value-of-cps exp* env* k*)))]
      [(*-let-k body env k)
       (let* ([exp* body]
              [env* (envr_extend-env v env)]
              [k* k])
         (value-of-cps exp* env* k*))]
      [(*-throw-k v-exp env)
       (let* ([exp* v-exp]
              [env* env]
              [v* v])
         (println v)
         (value-of-cps exp* env* v*))]
      [(*-rand-k r k)
       (let* ([r* r]
              [v* v]
              [k* k])
         (apply-closure r* v* k*))]
      [(*-rator-k rand env k)
       (let* ([exp* rand]
              [env* env]
              [k* (kt_*-rand-k v k)])
         (value-of-cps exp* env* k*))])))

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
