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

(define-program-counter pc)
(define-registers *e* *env* *k* *v* *p* *n*)

(define-label value-of-cps
  (union-case *e* expr
    [(const expr) (begin (set! *v* expr)
                         (set! pc app-k)
                         (pc))]
    [(mult exp1 exp2)
     (begin (set! *e* exp1)
            (set! *k* (kt_*-mult-outer-k exp2 *env* *k*))
            (set! pc value-of-cps)
            (pc))]
    [(sub1 exp) (begin (set! *e* exp)
                       (set! *k* (kt_*-sub1-k *k*))
                       (set! pc value-of-cps)
                       (pc))]
    [(zero exp) (begin (set! *e* exp)
                       (set! *k* (kt_*-zero-k *k*))
                       (set! pc value-of-cps)
                       (pc))]
    [(if test conseq alt)
     (begin (set! *e* test)
            (set! *k* (kt_*-if-k conseq alt *env* *k*))
            (set! pc value-of-cps)
            (pc))]
    [(let e body)
     (begin (set! *e* e)
            (set! *k* (kt_*-let-k body *env* *k*))
            (set! pc value-of-cps)
            (pc))]
    ;; we don't need let/cc to grap cont, because we are cps!
    [(letcc body)
     (begin (set! *e* body)
            (set! *env* (envr_extend-env *k* *env*))
            (set! pc value-of-cps)
            (pc))]
    [(throw k-exp v-exp)
     (begin (set! *e* k-exp)
            (set! *k* (kt_*-throw-k v-exp *env*))
            (set! pc value-of-cps)
            (pc))]
    [(var y) (begin (set! *n* y)
                    (set! pc apply-env)
                    (pc))]
    [(lambda body)
     (begin (set! *v* (clos_closure body *env*))
            (set! pc app-k)
            (pc))]
    [(app rator rand)
     (begin (set! *e* rator)
            (set! *k* (kt_*-rator-k rand *env* *k*))
            (set! pc value-of-cps)
            (pc))]))


(define-union envr
  (empty-env)
  (extend-env a env))

(define-union clos
  (closure body env))

(define-label apply-closure
  (union-case *p* clos
    [(closure body env)
     (begin (set! *e* body)
            (set! *env* (envr_extend-env *v* env))
            (set! pc value-of-cps)
            (pc))]))

(define-label apply-env
  (union-case *env* envr
    [(empty-env) (error 'value-of "unbound identifier")]
    [(extend-env a env)
     (if (zero? *n*)
         (begin (set! *v* a)
                (set! pc app-k)
                (pc))
         (begin (set! *n* (sub1 *n*))
                (set! *env* env)
                (set! pc apply-env)
                (pc)))]))

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


(define-label app-k
  (union-case *k* kt
    [(empty-k) *v*]
    [(*-mult-inner-k v^ k)
     (begin (set! *k* k)
            (set! *v* (* v^ *v*))
            (set! pc app-k)
            (pc))]
    [(*-mult-outer-k x2 env k)
     (begin (set! *e* x2)
            (set! *env* env)
            (set! *k* (kt_*-mult-inner-k *v* k))
            (set! pc value-of-cps)
            (pc))]
    [(*-sub1-k k)
     (begin (set! *k* k)
            (set! *v* (sub1 *v*))
            (set! pc app-k)
            (pc))]
    [(*-zero-k k)
     (begin (set! *k* k)
            (set! *v* (zero? *v*))
            (set! pc app-k)
            (pc))]
    [(*-if-k conseq alt env k)
     (if *v*
         (begin (set! *e* conseq)
                (set! *env* env)
                (set! *k* k)
                (set! pc value-of-cps)
                (pc))
         (begin (set! *e* alt)
                (set! *env* env)
                (set! *k* k)
                (set! pc value-of-cps)
                (pc)))]
    [(*-let-k body env k)
     (begin (set! *e* body)
            (set! *env* (envr_extend-env *v* env))
            (set! *k* k)
            (set! pc value-of-cps)
            (pc))]
    [(*-throw-k v-exp env)
     (begin (set! *e* v-exp)
            (set! *env* env)
            (set! *k* *v*) ; !
            (set! pc value-of-cps)
            (pc))]
    [(*-rand-k r k)
     (begin (set! *p* r)
            (set! *k* k)
            (set! pc apply-closure)
            (pc))]
    [(*-rator-k rand env k)
     (begin (set! *e* rand)
            (set! *env* env)
            (set! *k* (kt_*-rand-k *v* k))
            (set! pc value-of-cps)
            (pc))]))

;; (let ((f (lambda (f)
;;            (lambda (n)
;;              (if (zero? n)
;;                  1
;;                  (* n ((f f) (sub1 n))))))))
;;   (* (catch k ((f f) (throw k ((f f) 4)))) 5))

(define-label main
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
         (set! pc value-of-cps)
         (pc)))
