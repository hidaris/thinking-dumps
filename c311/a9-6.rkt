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

(define-registers *e* *env* *k* *v* *p* *n*)

(define value-of-cps
  (lambda ()
    (union-case *e* expr
      [(const expr) (begin (set! *v* expr)
                           (app-k))]
      [(mult exp1 exp2)
       (begin (set! *e* exp1)
              (set! *k* (kt_*-mult-outer-k exp2 *env* *k*))
              (value-of-cps))]
      [(sub1 exp) (begin (set! *e* exp)
                         (set! *k* (kt_*-sub1-k *k*))
                         (value-of-cps))]
      [(zero exp) (begin (set! *e* exp)
                         (set! *k* (kt_*-zero-k *k*))
                         (value-of-cps))]
      [(if test conseq alt)
       (begin (set! *e* test)
              (set! *k* (kt_*-if-k conseq alt *env* *k*))
              (value-of-cps))]
      [(let e body)
       (begin (set! *e* e)
              (set! *k* (kt_*-let-k body *env* *k*))
              (value-of-cps))]
      ;; we don't need let/cc to grap cont, because we are cps!
      [(letcc body)
       (begin (set! *e* body)
              (set! *env* (envr_extend-env *k* *env*))
              (value-of-cps))]
      [(throw k-exp v-exp)
       (begin (set! *e* k-exp)
              (set! *k* (kt_*-throw-k v-exp *env*))
              (value-of-cps))]
      [(var y) (begin (set! *n* y)
                      (apply-env))]
      [(lambda body)
       (begin (set! *v* (clos_closure body *env*))
              (app-k))]
      [(app rator rand)
       (begin (set! *e* rator)
              (set! *k* (kt_*-rator-k rand *env* *k*))
              (value-of-cps))])))


(define-union envr
  (empty-env)
  (extend-env a env))

(define-union clos
  (closure body env))

(define apply-closure
  (λ ()
    (union-case *p* clos
      [(closure body env)
       (begin (set! *e* body)
              (set! *env* (envr_extend-env *v* env))
              (value-of-cps))])))

(define apply-env
  (lambda ()
    (union-case *env* envr
      [(empty-env) (error 'value-of "unbound identifier")]
      [(extend-env a env)
       (if (zero? *n*)
           (begin (set! *v* a)
                  (app-k))
           (begin (set! *n* (sub1 *n*))
                  (set! *env* env)
                  (apply-env)))])))

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
  (lambda ()
    (union-case *k* kt
      [(empty-k) *v*]
      [(*-mult-inner-k v^ k)
       (begin (set! *k* k)
              (set! *v* (* v^ *v*))
              (app-k))]
      [(*-mult-outer-k x2 env k)
       (begin (set! *e* x2)
              (set! *env* env)
              (set! *k* (kt_*-mult-inner-k *v* k))
              (value-of-cps))]
      [(*-sub1-k k)
       (begin (set! *k* k)
              (set! *v* (sub1 *v*))
              (app-k))]
      [(*-zero-k k)
       (begin (set! *k* k)
              (set! *v* (zero? *v*))
              (app-k))]
      [(*-if-k conseq alt env k)
       (if *v*
           (begin (set! *e* conseq)
                  (set! *env* env)
                  (set! *k* k)
                  (value-of-cps))
           (begin (set! *e* alt)
                  (set! *env* env)
                  (set! *k* k)
                  (value-of-cps)))]
      [(*-let-k body env k)
       (begin (set! *e* body)
              (set! *env* (envr_extend-env *v* env))
              (set! *k* k)
              (value-of-cps))]
      [(*-throw-k v-exp env)
       (begin (set! *e* v-exp)
              (set! *env* env)
              (set! *k* *v*) ; !
              (value-of-cps))]
      [(*-rand-k r k)
       (begin (set! *p* r)
              (set! *k* k)
              (apply-closure))]
      [(*-rator-k rand env k)
       (begin (set! *e* rand)
              (set! *env* env)
              (set! *k* (kt_*-rand-k *v* k))
              (value-of-cps))])))

;; (let ((f (lambda (f)
;;            (lambda (n)
;;              (if (zero? n)
;;                  1
;;                  (* n ((f f) (sub1 n))))))))
;;   (* (catch k ((f f) (throw k ((f f) 4)))) 5))

(define main
  (lambda ()
    (begin (set! *e* (expr_let
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
                        (expr_const 5))))
           (set! *env* (envr_empty-env))
           (set! *k* (kt_empty-k))
           (value-of-cps))))
