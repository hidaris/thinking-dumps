#lang racket

(define ack
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))]
      [(zero? n) (ack (sub1 m) 1 k)]
      [else (ack m (sub1 n) (lambda (v) (ack (sub1 m) v k)))])))

(define m* 'm)
(define n* 'n)
(define k* 'k)

(define ack-reg
  (lambda ()
    (cond
      [(zero? m*) (begin (set! v* (add1 n*))
                         (app-k-reg-ack))]
      [(zero? n*) (begin (set! m* (sub1 m*))
                         (set! n* 1)
                         (ack-reg))]
      [else (begin (set! n* (sub1 n*))
                   (set! k* (*-ack-inner-k m* k*))
                   (ack-reg))])))

(define ack-driver
  (lambda (m n)
    (begin
      (set! m* m)
      (set! n* n)
      (set! k* (empty-k))
      (ack-reg))))

(define *-ack-inner-k
  (lambda (m k)
    `(*-ack-inner-k ,m ,k)))

(define empty-k
  (lambda ()
    `(empty-k)))

(define v* 'v)

(define app-k-reg-ack
  (lambda ()
    (match k*
      [`(empty-k) v*]
      [`(*-ack-inner-k ,m ,k^)
       (begin (set! m* (sub1 m))
              (set! n* v*)
              (set! k* k^)
              (ack-reg))])))

(define depth
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(pair? (car ls))
       (depth (car ls)
              (lambda (l)
                (depth (cdr ls)
                       (lambda (r)
                         (let ((l (add1 l)))
                           (if (< l r) (k r) (k l)))))))]
      [else (depth (cdr ls) k)])))

(define ls* 'ls)

(define depth-reg
  (lambda ()
    (cond
      [(null? ls*) (begin (set! v* 1)
                          (app-k-reg-depth))]
      [(pair? (car ls*))
       (begin (set! k* (*-depth-reg-outer-k ls* k*))
              (set! ls* (car ls*)))
       (depth-reg)]
      [else (begin (set! ls* (cdr ls*))
                   (depth-reg))])))

(define *-depth-reg-outer-k
  (lambda (ls k)
    `(*-depth-reg-outer-k ,ls ,k)))

(define *-depth-reg-inner-k
  (lambda (l k)
    `(*-depth-reg-inner-k ,l ,k)))

(define app-k-reg-depth
  (lambda ()
    (match k*
      [`(empty-k) v*]
      [`(*-depth-reg-inner-k ,l ,k)
       (let ((l (add1 l)))
         (if (< l v*)
             (begin (set! k* k)
                    (app-k-reg-depth))
             (begin (set! k* k)
                    (set! v* l)
                    (app-k-reg-depth))))]
      [`(*-depth-reg-outer-k ,ls ,k)
       (begin (set! ls* (cdr ls))
              (set! k* (*-depth-reg-inner-k v* k)))
       (depth-reg)])))

(define depth-driver
  (lambda (ls)
    (begin (set! ls* ls)
           (set! k* (empty-k))
           (depth-reg))))

(define binary-to-decimal-reg
  (lambda ()
    (cond
      [(null? n*) (begin (set! v* 0)
                         (app-k-binary))]
      [else (begin (set! k* (*-binary-to-decimal-inner-k n* k*))
                   (set! n* (cdr n*))
                   (binary-to-decimal-reg))])))

(define *-binary-to-decimal-inner-k
  (lambda (n k)
    `(*-binary-to-decimal-inner-k ,n ,k)))

(define app-k-binary
  (lambda ()
    (match k*
      [`(empty-k) v*]
      [`(*-binary-to-decimal-inner-k ,n ,k)
       (begin (set! k* k)
              (set! v* (+ (car n) (* 2 v*)))
              (app-k-binary))])))

(define binary-to-decimal-driver
  (lambda (n)
    (begin (set! n* n)
           (set! k* (empty-k))
           (binary-to-decimal-reg))))


(define fib
  (lambda ()
    (cond
      [(zero? n*) (begin (set! v* 0)
                         (app-k-fib))]
      [(zero? (sub1 n*)) (begin (set! v* 1)
                                (app-k-fib))]
      [else (begin (set! k* (*-fib-reg-outer-k n* k*))
                   (set! n* (sub1 n*))
                   (fib))])))

(define *-fib-reg-outer-k
  (lambda (n k)
    `(*-fib-reg-outer-k ,n ,k)))

(define *-fib-reg-inner-k
  (lambda (fibsub1n k)
    `(*-fib-reg-inner-k ,fibsub1n ,k)))

(define app-k-fib
  (lambda ()
    (match k*
      [`(empty-k) v*]
      [`(*-fib-reg-outer-k ,n ,k)
       (begin (set! n* (sub1 (sub1 n)))
              (set! k* (*-fib-reg-inner-k v* k)))
       (fib)]
      [`(*-fib-reg-inner-k ,n ,k)
       (begin (set! k* k)
              (set! v* (+ n v*)))
       (app-k-fib)])))

(define fib-driver
  (lambda (n)
    (begin (set! n* n)
           (set! k* (empty-k))
           (fib))))

(define a* 'a)

(define pascal
  (lambda ()
    (begin (set! v* (lambda ()
                      (cond
                        [(> m* n*) (begin (set! v* '())
                                          (app-k-pascal))]
                        [else (begin (set! a* (+ a* m*))
                                     (set! k* (*-pascal-outer-k m* a* k*))
                                     (pascal))])))
           (app-k-pascal))))

(define *-k0
  (lambda (k)
    `(*-k0 ,k)))

(define *-pascal-outer-k
  (lambda (m a k)
    `(*-pascal-outer-k ,m ,a ,k)))

(define *-pascal-inner-k
  (lambda (a k)
    `(*-pascal-inner-k ,a ,k)))

(define app-k-pascal
  (lambda ()
    (match k*
      [`(empty-k) v*]
      [`(*-k0 ,k)
       (begin (set! m* 1)
              (set! a* 0)
              (set! k* k)
              (v*))]
      [`(*-pascal-outer-k ,m ,a ,k)
       (begin (set! m* (add1 m))
              (set! a* a)
              (set! k* (*-pascal-inner-k a k)))
       (v*)]
      [`(*-pascal-inner-k ,a ,k)
       (begin (set! k* k)
              (set! v* (cons a v*))
              (app-k-pascal))])))

(define pascal-driver
  (lambda (n)
    (begin (set! n* n)
           (set! k* (*-k0 (empty-k)))
           (pascal))))

(define pc* 'pc)

(define fib-ramp-origin
  (lambda (n k)
    (cond
      [(and (not (negative? n)) (< n 2)) (k n)]
      [else (fib-ramp-origin (sub1 n)
                             (lambda (v^)
                               (fib-ramp-origin (sub1 (sub1 n))
                                                (lambda (v)
                                                  (k (+ v^ v))))))])))


(define fib-ramp
  (lambda ()
    (cond
      [(and (not (negative? n*)) (< n* 2)) (begin (set! v* n*)
                                                  (set! pc* app-k-fib-ramp))]
      [else
       (begin (set! k* (*-fib-ramp-outer-k n* k*))
              (set! n* (sub1 n*))
              (set! pc* fib-ramp))])))

(define *-fib-ramp-inner-k
  (lambda (x k)
    `(*-fib-ramp-inner-k ,x ,k)))

(define *-fib-ramp-outer-k
  (lambda (n k)
    `(*-fib-ramp-outer-k ,n ,k)))

(define done* #f)
(define ramp-empty-k
  (lambda (k)
    `(empty-k ,k)))

(define app-k-fib-ramp
  (lambda ()
    (match k*
      [`(empty-k ,k) (k v*) ;(set! done* #t)
       ]
      [`(*-fib-ramp-inner-k ,x ,k)
       (begin (set! k* k)
              (set! v* (+ x v*))
              (set! pc* app-k-fib-ramp))]
      [`(*-fib-ramp-outer-k ,n ,k)
       (begin (set! n* (sub1 (sub1 n)))
              (set! k* (*-fib-ramp-inner-k v* k))
              (set! pc* fib-ramp))])))

(define trampoline
  (lambda ()
    (if done*
        v*
        (begin (pc*)
               (trampoline)))))

(define fib-ramp-driver
  (lambda (n)
    (begin (set! n* n)
           (set! k* (empty-k))
           (set! done* #f)
           (set! pc* fib-ramp)
           (trampoline))))
