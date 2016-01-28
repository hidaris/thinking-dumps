#lang racket

(define counter 0)
(define set-counter 0)
;;; we can use (counter) to refer N
(define konsC
  (let ((N 0))
    (set! counter
          (lambda ()
            N))
    (set! set-counter
          (lambda (x)
            (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (kons2 x y))))


(define add-at-end
  (lambda (l)
    (cond
      ((null? (bdr l))
       (konsC (bar l)
              (konsC (quote egg)
                     (quote ()))))
      (else (konsC (bar l)
                   (add-at-end (bdr l)))))))

(define add-at-end-too
  (lambda (l)
    (letrec
        ((A (lambda (ls)
              (cond
                ((null? (bdr ls))
                 (set-bdr ls
                          (kons2 (quote egg)
                                 (quote ()))))
                (else (A (bdr ls)))))))
      (A l)
      l)))

;;; encode cons, car and cdr
(define kons
  (λ (first rest)
    (λ (selector)
      (selector first rest))))

(define kar
  (λ (c)
    (c (λ (a d) a))))

(define kdr
  (λ (c)
    (c (λ (a d) d))))

;;; the definition of bons
(define bons
  (λ (first)
    (let ((rest (quote ())))
      (λ (selector)
        (selector
         (λ (x) (set! rest x))
         first
         rest)))))

(define bar
  (λ (c)
    (c (λ (s a d) a))))

(define bdr
  (λ (c)
    (c (λ (s a d) d))))

(define set-bdr
  (λ (c x)
    ((c (λ (s a d) s)) x)))

(define kons2
  (λ (a d)
    (let ((c (bons a)))
      (set-bdr c d)
      c)))

(define lots
  (lambda (m)
    (cond
      ((zero? m) (quote ()))
      (else (kons2 (quote egg)
                   (lots (sub1 m)))))))

(define lenkth
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (lenkth (bdr l)))))))

(define dozen
  (lots 12))

(define bakers-dozen
  (add-at-end dozen))

(define bakers-dozen-too
  (add-at-end-too dozen))

(define bakers-dozen-again
  (add-at-end dozen))

(define eklist?
  (λ (ls1 ls2)
    (cond
      ((null? ls1) (null? ls2))
      ((null? ls2) #f)
      (else
       (and (eq? (bar ls1) (bar ls2))
            (eklist? (bdr ls1) (bdr ls2)))))))

(define same?
  (λ (c1 c2)
    (let ((t1 (bdr c1))
          (t2 (bdr c2)))
      (set-bdr c1 1)
      (set-bdr c2 2)
      (let ((v (= (bdr c1) (bdr c2))))
        (set-bdr c1 t1)
        (set-bdr c2 t2)
        v))))

(define last-kons
  (λ (ls)
    (cond
      ((null? (bdr ls)) ls)
      (else (last-kons (bdr ls))))))

(define long (lots 12))

(define finite-lenkth
  (λ (p)
    (let/cc infinite
      (letrec
          ((C (λ (p q)
                (cond
                  ((same? p q)
                   (infinite #f))
                  ((null? q) 0)
                  ((null? (bdr q)) 1)
                  (else
                   (+ (C (sl p) (qk q))
                      2)))))
           (qk (λ (x) (bdr (bdr x))))
           (sl (λ (x) (bdr x))))
        (cond
          ((null? p) 0)
          (else
           (add1 (C p (bdr p)))))))))

;;; Guy's favorite pie
(define mongo
  (kons2 (quote pie)
         (kons2 (quote a)
                (kons2 (quote la)
                       (kons2 (quote mode)
                              (quote ()))))))
(set-bdr (bdr (bdr (bdr mongo))) (bdr mongo))
