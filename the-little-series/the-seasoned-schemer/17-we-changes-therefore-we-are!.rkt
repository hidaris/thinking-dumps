#lang racket

;;; write deep with if
(define deep
  (lambda (m)
    (if (zero? m)
        (quote pizza)
        (cons (deep (sub1 m))
              (quote ())))))

;;; a deepM with the new version of deep
(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ()))
        (D (lambda (m)
             (if (zero? m)
                 (quote pizza)
                 (cons (deepM (sub1 m))
                       (quote ()))))))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (D n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

;;; Since the definition does not contain
;;; (set! D ...) and D is used in only one place,
;;; we can replace D by its value:
(define deepM2
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result ((lambda (m)
                             (if (zero? m)
                                 (quote pizza)
                                 (cons (deepM2 (sub1 m))
                                       (quote ()))))
                           n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

;;; applying (lambda ...) immediately to an argument is equivalent to
;;; (let ...)
(define deepM3
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result
                   (let ((m n))
                     (if (zero? m)
                         (quote pizza)
                         (cons (deepM3 (sub1 m))
                               (quote ()))))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

;;; we could unname again.
(define deepM4
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result
                   (if (zero? n)
                       (quote pizza)
                       (cons (deepM4 (sub1 n))
                             (quote ())))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

;;; helper
(define atom?
  (lambda (a)
    (and (not (null? a)) (not (pair? a)))))

(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond
                ((null? ns) #f)
                ((= (car ns) n) (car rs))
                (else
                 (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

;;; write a function consC which returns
;;; the same value as cons and counts how
;;; many times it sees arguments.

;;; This is no different from writing deepR except
;;; that we use add1 to build a number rather than
;;; cons to build a list.
;; (define consC
;;   (let ((N 0))
;;     (lambda (x y)
;;       (set! N (add1 N))
;;       (cons x y))))

;; (define deepC
;;   (lambda (m)
;;     (if (zero? m)
;;         (quote pizza)
;;         (consC (deepC (sub1 m))
;;                (quote ())))))

;;; But N is define in (let ...)
;;; How could we possibly see something
;;; that is imaginary?
(define counter 0)
(define set-counter 0)
;;; we can use (counter) to refer N
(define consC
  (let ((N 0))
    (set! counter
          (lambda ()
            N))
    (set! set-counter
          (lambda (x)
            (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

;;; use deepM to avoid repeat
(define deepC
  (lambda (m)
    (if (zero? m)
        (quote pizza)
        (consC (deepM5 (sub1 m))
               (quote ())))))

(define supercounter
  (lambda (f)
    (letrec
        ((S (lambda (n)
              (if (zero? n)
                  (f n)
                  (let ()
                    (f n)
                    (S (sub1 n)))))))
      (S 1000)
      (counter))))

(define deepM5
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result
                   (if (zero? n)
                       (quote pizza)
                       (consC
                        (deepM5 (sub1 n))
                        (quote ())))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

;;; in the test, the prev code still has a problem.


;;; Here is rember1* again:
(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l oh)
              (cond
                ((null? l)
                 (oh (quote no)))
                ((atom? (car l))
                 (if (eq? (car l) a)
                     (cdr l)
                     (cons (car l)
                           (R (cdr l) oh))))
                (else
                 (let ((new-car
                        (let/cc oh
                          (R (car l)
                             oh))))
                   (if (atom? new-car)
                       (cons (car l)
                             (R (cdr l) oh))
                       (cons new-car
                             (cdr l)))))))))
      (let ((new-l (let/cc oh (R l oh))))
        (if (atom? new-l)
            l
            new-l)))))

(define rember1*C
  (lambda (a l)
    (letrec
        ((R (lambda (l oh)
              (cond
                ((null? l)
                 (oh (quote no)))
                ((atom? (car l))
                 (if (eq? (car l) a)
                     (cdr l)
                     (consC (car l)
                           (R (cdr l) oh))))
                (else
                 (let ((new-car
                        (let/cc oh
                          (R (car l)
                             oh))))
                   (if (atom? new-car)
                       (consC (car l)
                             (R (cdr l) oh))
                       (consC new-car
                             (cdr l)))))))))
      (let ((new-l (let/cc oh (R l oh))))
        (if (atom? new-l)
            l
            new-l)))))

;;; the first good version of rember1*
(define rember1*2
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) (quote ()))
                ((atom? (car l))
                 (if (eq? (car l) a)
                     (cdr l)
                     (cons (car l)
                           (R (cdr l)))))
                (else
                 (let ((av (R (car l))))
                   (if (eqlist? (car l) av)
                       (cons (car l)
                             (R (cdr l)))
                       (cons av
                             (cdr l)))))))))
      (R l))))

(define rember1*2C
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) (quote ()))
                ((atom? (car l))
                 (if (eq? (car l) a)
                     (cdr l)
                     (consC (car l)
                           (R (cdr l)))))
                (else
                 (let ((av (R (car l))))
                   (if (eqlist? (car l) av)
                       (consC (car l)
                             (R (cdr l)))
                       (consC av
                             (cdr l)))))))))
      (R l))))


(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2))
       #t)
      ((or (null? l1) (null? l2))
       #f)
      (else (and (equal? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))))))
