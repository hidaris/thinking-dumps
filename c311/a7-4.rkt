#lang racket


(define lex
  (λ (e acc)
    (match e
      [`,n #:when (number? n)
           `(const ,n)]
      [`,y #:when (symbol? y)
           `(var ,(index-of acc y))]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       `(lambda ,(lex body (cons x acc)))]
      [`(zero? ,y)
       `(zero ,(lex y acc))]
      [`(* ,e1 ,e2)
       `(mult ,(lex e1 acc) ,(lex e2 acc))]
      [`(let/cc ,k ,body)
       `(letcc ,(lex body (cons k acc)))]
      [`(throw ,e1 ,e2)
       `(throw ,(lex e1 acc) ,(lex e2 acc))]
      [`(,rator ,rand)
       `(app ,(lex rator acc) ,(lex rand acc))])))


(define value-of-cps
  (lambda (expr env k)
    (match expr
      [`(const ,expr) (app-k k expr)]
      [`(mult ,x1 ,x2)
       (value-of-cps x1 env (*-mult-outer-k x2 env k))]
      [`(sub1 ,x) (value-of-cps x env (*-sub1-k k))]
      [`(zero ,x) (value-of-cps x env (*-zero-k k))]
      [`(if ,test ,conseq ,alt)
       (value-of-cps test env (*-if-k conseq alt env k))]
      [`(let ,e ,body)
       (value-of-cps e env (*-let-k body env k))]
      ;; we don't need let/cc to grap cont, because we are cps!
      [`(letcc ,body) (value-of-cps body (extend-env k env) k)]
      [`(throw ,k-exp ,v-exp)
       (value-of-cps k-exp env (*-throw-k v-exp env))]
      [`(var ,y) (apply-env env y k)]
      [`(lambda ,body)
       (app-k k (closure body env))]
      [`(app ,rator ,rand)
       (value-of-cps rator env (*-rator-k rand env k))])))

(define empty-env
  (lambda ()
    `(empty-env)))

(define extend-env
  (lambda (a env)
    `(ext-env ,a ,env)))

(define closure
  (lambda (body env)
    `(closure ,body ,env)))

(define apply-closure
  (λ (p a k)
    (match p
      [`(closure ,body ,env)
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

(require racket/trace)
;; (trace value-of-cps)

(module+ test
  (require rackunit)

  (check-equal? (value-of-cps '(mult (const 5) (const 5)) (empty-env) (empty-k)) 25)
  (check-equal? (value-of-cps '(sub1 (sub1 (const 5))) (empty-env) (empty-k)) 3)
  (check-equal? (value-of-cps '(if (zero (const 0)) (mult (const 2) (const 2)) (const 3)) (empty-env) (empty-k)) 4)
  (check-equal? (value-of-cps '(app (app (lambda (lambda (var 1))) (const 6)) (const 5)) (empty-env) (empty-k)) 6)
  (check-equal? (value-of-cps '(app (lambda (app (lambda (var 1)) (const 6))) (const 5)) (empty-env) (empty-k)) 5)
  (check-equal? (value-of-cps '(let (const 6) (const 4)) (empty-env) (empty-k)) 4)
  (check-equal? (value-of-cps '(let (const 5) (var 0)) (empty-env) (empty-k)) 5)
  (check-equal? (value-of-cps '(mult (const 5) (let (const 5) (var 0))) (empty-env) (empty-k)) 25)
  (check-equal? (value-of-cps '(app (if (zero (const 4)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 5)
  (check-equal? (value-of-cps '(app (if (zero (const 0)) (lambda (var 0)) (lambda (const 5))) (const 3)) (empty-env) (empty-k)) 3)
  (check-equal? (value-of-cps '(letcc (throw (throw (var 0) (const 5)) (const 6))) (empty-env) (empty-k)) 5)
  (check-equal? (value-of-cps '(letcc (throw (const 5) (throw (var 0) (const 5)))) (empty-env) (empty-k)) 5)
  (check-equal? (value-of-cps '(mult (const 3) (letcc (throw (const 5) (throw (var 0) (const 5))))) (empty-env) (empty-k)) 15)
  (check-equal? (value-of-cps '(if (zero (const 5)) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))) (const 4))
                              (empty-env)
                              (empty-k))
                4)
  (check-equal? (value-of-cps '(if (zero (const 0)) (const 4) (app (lambda (app (var 0) (var 0))) (lambda (app (var 0) (var 0)))))
                              (empty-env)
                              (empty-k))
                4)
  (check-equal? (value-of-cps '(app (lambda (app (app (var 0) (var 0)) (const 2)))
                                    (lambda
                                        (lambda
                                            (if (zero (var 0))
                                                (const 1)
                                                (app (app (var 1) (var 1)) (sub1 (var 0)))))))
                              (empty-env)
                              (empty-k))
                1))
