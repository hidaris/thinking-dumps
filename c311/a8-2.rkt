#lang racket

(define fib-ramp
  (lambda (n k)
    (cond
      [(and (not (negative? n)) (< n 2)) (app-k k n)]
      [else (fib-ramp (sub1 n)
                      (*-fib-ramp-outer-k n k))])))

(define *-fib-ramp-inner-k
  (lambda (x k)
    `(*-fib-ramp-inner-k ,x ,k)))

(define *-fib-ramp-outer-k
  (lambda (n k)
    `(*-fib-ramp-outer-k ,n ,k)))

(define ramp-empty-k
  (lambda (jumpout)
    `(ramp-empty-k ,jumpout)))

(define app-k
  (lambda (k v)
    (match k
      [`(ramp-empty-k ,jumpout) (jumpout v)]
      [`(*-fib-ramp-outer-k ,n ,k)
       (fib-ramp (sub1 (sub1 n))
                 (*-fib-ramp-inner-k v k))]
      [`(*-fib-ramp-inner-k ,x ,k)
       (app-k k (+ x v))])))

(define rampoline
  (lambda (th1 th2 th3)
    (let [(n (random 3))]
      (cond
        [(= n 0) (rampoline (th1) th2 th3)]
        [(= n 1) (rampoline th1 (th2) th3)]
        [(= n 2) (rampoline th1 th2 (th3))]))))

(define fib-ramp-driver
  (lambda (n1 n2 n3)
    (let/cc jumpout
      (rampoline
       (lambda ()
         (fib-ramp n1 (ramp-empty-k jumpout)))
       (lambda ()
         (fib-ramp n2 (ramp-empty-k jumpout)))
       (lambda ()
         (fib-ramp n3 (ramp-empty-k jumpout)))))))


(define trib
  (lambda (n k)
    (cond
      [(< n 3) (k 1)]
      [else
       (trib (- n 3)
             (*-trib-k3 n k))])))

(define empty-k
  (lambda ()
    (lambda (v)
      v)))

(define trib-empty-k
  (lambda (n k)
    (lambda (v)
      (k `(,n . ,v)))))

(define *-trib-k1
  (lambda (x y k)
    (lambda (z)
      (k (+ x y z)))))

(define *-trib-k2
  (lambda (n x k)
    (lambda (y)
      (trib (- n 1)
            (*-trib-k1 x y k)))))

(define *-trib-k3
  (lambda (n k)
    (lambda (x)
      (trib (- n 2)
            (*-trib-k2 n x k)))))

(define trampoline
  (lambda (th1 th2)
    (let ([r1 (th1)]
          [r2 (th2)])
      (cond
        [(and (number? r1) (number? r2))
         (list r1 r2)]
        [(number? r1)
         (trampoline (lambda () r1) r2)]
        [(number? r2)
         (trampoline (lambda () r2) r1)]
        [else (trampoline r1 r2)]))))


(define bi-tramp-driver
  (lambda (n1 n2)
    (trampoline
     (lambda () (trib n1 (empty-k)))
     (lambda () (trib n2 (empty-k))))))


(define bi-trampoline
  (lambda ths
    (apply list (map force ths))))

(define bi-tramp-driver2
  (lambda (n1 n2)
    (bi-trampoline
     (delay (trib n1 (empty-k)))
     (delay (trib n2 (empty-k))))))

(define trampoline3
  (lambda (th1 th2)
    (let [(n (random 2))]
      (cond
        [(= n 0) (trampoline3 (th1 n) th2)]
        [(= n 1) (trampoline3 th1 (th2 n))]))))

(define bi-tramp-driver-inner
  (lambda (n1 n2)
    (let/cc jumpout
      (trampoline3
       (lambda (n)
         (trib n1 (trib-empty-k n jumpout)))
       (lambda (n)
         (trib n2 (trib-empty-k n jumpout)))))))

(define bi-tramp-driver3
  (lambda (n1 n2)
    (let ([t '()]
          [r '()]
          [i (bi-tramp-driver-inner n1 n2)])
      (let loop ()
        (when (< (length r) 2)
          (if (not (memq (car i) t))
              (begin (set! t (cons (car i) t))
                     (set! r (cons (cdr i) r)))
              #f)
          (set! i (bi-tramp-driver-inner n1 n2))
          (loop)))
      r)))
