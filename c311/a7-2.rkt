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
      [`(const ,expr) (k expr)]
      [`(mult ,x1 ,x2)
       (value-of-cps x1 env
                     (lambda (x)
                       (value-of-cps x2 env
                                     (lambda (x^)
                                       (k (* x x^))))))]
      [`(sub1 ,x) (value-of-cps x env
                                (lambda (x^)
                                  (k (sub1 x^))))]
      [`(zero ,x) (value-of-cps x env
                                (lambda (x^)
                                  (k (zero? x^))))]
      [`(if ,test ,conseq ,alt)
       (value-of-cps test env
                     (lambda (x)
                       (if x
                           (value-of-cps conseq env k)
                           (value-of-cps alt env k))))]
      [`(let ,e ,body)
       (value-of-cps e env
                     (lambda (x)
                       ((lambda (a k^)
                          (value-of-cps body (extend-env a env) k^))
                        x
                        k)))]
      ;; we don't need let/cc to grap cont, because we are cps!
      [`(letcc ,body) (value-of-cps body (extend-env k env) k)]
      [`(throw ,k-exp ,v-exp)
       (value-of-cps k-exp env
                     (lambda (c)
                       (value-of-cps v-exp env
                                     (lambda (v)
                                       (c v)))))]
      [`(var ,y) (apply-env env y k)]
      [`(lambda ,body)
       (k (closure body env))]
      [`(app ,rator ,rand)
       (value-of-cps rator env
                     (lambda (x)
                       (value-of-cps rand env
                                     (lambda (y)
                                       (apply-closure x y k)))))])))
 
(define empty-env
  (lambda ()
    (lambda (y)
      (error 'value-of "unbound identifier"))))

(define extend-env
  (lambda (a env)
    (lambda (n k)
      (if (zero? n)
          (k a)
          (env (sub1 n) k)))))

(define closure
  (lambda (body env)
    (lambda (a k)
      (value-of-cps body (extend-env a env) k))))


(define apply-closure
  (λ (p a k)
    (p a k)))

(define apply-env
  (lambda (env n k)
    (env n k)))
 
(define empty-k
  (lambda ()
    (lambda (v)
      v)))

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
