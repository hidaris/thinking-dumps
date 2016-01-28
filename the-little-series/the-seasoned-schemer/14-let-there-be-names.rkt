#lang racket/base

;; a function that extracts the leftmost
;; atom from a list of S-expressions.
;; in this version, there is no answer while
;; the leftmost argument is empty.
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

(define leftmost-b
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (car l))
      (else (cond
              ((atom? (leftmost-b (car l)))
               (leftmost-b (car l)))
              (else (leftmost-b (cdr l))))))))

(define leftmost-let
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (car l))
      (else
       (let ((a (leftmost-let (car l))))
         (cond
           ((atom? a) a)
           (else (leftmost-let (cdr l)))))))))
;;; like (and ...), (let ...) is an abbreviation:
;;;      (let ((x1 a1) (x2 a2) ...) β ...)
;;;      = ((λ (x1 x2 ...) β) a1 a2 ...)

(define rember1*
  (lambda (a l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (cdr l))
         (else (cons (car l)
                     (rember1* a (cdr l))))))
      (else
       (cond
         ((eqlist?
           (rember1* a (car l))
           (car l))
          (cons (car l)
                (rember1* a (cdr l))))
         (else (cons (rember1* a (car l))
                     (cdr l))))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2))
       #t)
      ((or (null? l1) (null? l2))
       #f)
      (else (and (equal? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))))))

(define rember2*
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) (quote ()))
                ((atom? (car l))
                 (cond
                   ((eq? (car l) a) (cdr l))
                   (else (cons (car l)
                               (R (cdr l))))))
                (else
                 (let ((av (R (car l))))
                   (cond
                     ((eqlist? av (car l))
                      (cons (car l)
                            (R (cdr l))))
                     (else (cons av (cdr l))))))))))
      (R l))))
;;; the fifteenth commandment
;;; preliminary version
;;; use (let ...) to name the values of repeated expressions.

;; (define depth*
;;   (lambda (l)
;;     (let ((a (add1 (depth* (car l))))
;;           (d (depth* (cdr l))))
;;       (cond
;;         ((null? l) 1)
;;         ((atom? (car l)) d)
;;         (else (cond
;;                 ((> d a) d)
;;                 (else a)))))))

(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth* (cdr l)))
      (else
       (let ((a (add1 (depth* (car l))))
             (d (depth* (cdr l))))
         (cond
           ((> d a) d)
           (else a)))))))

;;; the new version of depth* is harder to read.
(define depth2*
  (lambda (l)
    (cond
      ((null? l) 1)
      (else
       (let ((d (depth2* (cdr l))))
         (cond
           ((atom? (car l)) d)
           (else
            (let ((a (add1 (depth2* (car l)))))
              (cond
                ((> d a) d)
                (else a))))))))))
;;; The Fifteenth commandment
;;; Use (let ...) to name the values of repeated expressions
;;; in a function definition if they may be evaluated twice for
;;; one and the same use of the function.

;;; make it more simpler.
(define depth3*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth3* (cdr l)))
      (else
       (let ((a (add1 (depth3* (car l))))
             (d (depth3* (cdr l))))
         (if (> d a) d a))))))
;;; (if ...) asks only one question and provides
;;; two answers: if the question is true, it selects
;;; the first answer; otherwise, it selects the second
;;; answer.
;;; Like (and ...), (if ...) can be abbreviated:
;;; (if α β z) = (cond (α β) (else z))

(define depth4*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth4* (cdr l)))
      (else
       (let ((a (add1 (depth4* (car l))))
             (d (depth4* (cdr l))))
         (max a d))))))

(define depth5*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth5* (cdr l)))
      (else (max
             (add1 (depth5* (car l)))
             (depth5* (cdr l)))))))

;;; more letting practice.
(define scramble
  (lambda (tup)
    (letrec
        ((P (lambda (tup rp)
              (cond
                ((null? tup) (quote ()))
                (else
                 (let ((rp (cons (car tup) rp)))
                   (cons (pick (car tup) rp)
                         (P (cdr tup) rp)))))))
         (pick (lambda (n lat)
                 (cond
                   ((one? n) (car lat))
                   (else (pick (sub1 n) (cdr lat))))))
         (one? (lambda (n)
                 (zero? (sub1 n)))))
      (P tup (quote ())))))

;; (define leftm-let
;;   (lambda (l)
;;     (cond
;;       ((null? l) (quote ()))
;;       ((atom? (car l)) (car l))
;;       (else
;;        (let ((a (leftm-let (car l))))
;;          (cond
;;            ((atom? a) a)
;;            (else (leftm-let (cdr l)))))))))

(define leftmost-cc
  (lambda (l)
    (let/cc skip
      (lm l skip))))

(define lm
  (lambda (l out)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (out (car l)))
      (else (let ()
              (lm (car l) out)
              (lm (cdr l) out))))))
;;; let (), begin also works. progn also work in cl.

(define leftmost-final
  (letrec
      ((lm (lambda (l out)
             (cond
               ((null? l) (quote ()))
               ((atom? (car l))
                (out (car l)))
               (else (let ()
                       (lm (car l) out)
                       (lm (cdr l) out)))))))
    (lambda (l)
      (let/cc skip
        (lm l skip)))))

(define leftmost-better
  (lambda (l)
    (letrec
        ((lm (lambda (l out)
               (cond
                 ((null? l) (quote ()))
                 ((atom? (car l))
                  (out (car l)))
                 (else (let ()
                         (lm (car l) out)
                         (lm (cdr l) out)))))))
      (let/cc skip
        (lm l skip)))))

(define leftmost-more-better
  (lambda (l)
    (let/cc skip
      (letrec
          ((lm (lambda (l out)
                 (cond
                   ((null? l) (quote ()))
                   ((atom? (car l))
                    (out (car l)))
                   (else (let ()
                           (lm (car l) out)
                           (lm (cdr l) out)))))))
        (lm l skip)))))

(define leftmost-more-better-b
  (lambda (l)
    (let/cc skip
      (letrec
          ((lm (lambda (l skip)
                 (cond
                   ((null? l) (quote ()))
                   ((atom? (car l))
                    (skip (car l)))
                   (else
                    (let ()
                      (lm (car l) skip)
                      (lm (cdr l) skip)))))))
        (lm l skip)))))

(define leftmost-more-better-c
  (lambda (l)
    (let/cc skip
      (letrec
          ((lm (lambda (l)
                 (cond
                   ((null? l) (quote ()))
                   ((atom? (car l))
                    (skip (car l)))
                   (else
                    (let ()
                      (lm (car l))
                      (lm (cdr l))))))))
        (lm l)))))

;; (define rember2*
;;   (lambda (a l)
;;     (letrec
;;         ((R (lambda (l)
;;               (cond
;;                 ((null? l) (quote ()))
;;                 ((atom? (car l))
;;                  (cond
;;                    ((eq? (car l) a) (cdr l))
;;                    (else (cons (car l)
;;                                (R (cdr l))))))
;;                 (else
;;                  (let ((av (R (car l))))
;;                    (cond
;;                      ((eqlist? av (car l))
;;                       (cons (car l)
;;                             (R (cdr l))))
;;                      (else (cons av (cdr l))))))))))
;;       (R l))))

(define rm
  (lambda (a l oh)
    (cond
      ((null? l) (oh (quote no)))
      ((atom? (car l))
       (if (eq? (car l) a)
           (cdr l)
           (cons (car l)
                 (rm a (cdr l) oh))))
      (else
       (if (atom?
            (let/cc oh
              (rm a (car l) oh)))
           (cons (car l)
                 (rm a (cdr l) oh))
           (cons (rm a (car l) 0)
                 (cdr l)))))))

(define rember3*
  (lambda (a l)
    (if (atom? (let/cc oh (rm a l oh)))
        l
        (rm a l (quote ())))))

;;; or name the values of some expression in rember
(define rember4*
  (lambda (a l)
    (let ((new-l (let/cc oh (rm a l oh))))
      (if (atom? new-l)
          l
          new-l))))

;;; We can also use (let ...) in rm:
(define rm2
  (lambda (a l oh)
    (cond
      ((null? l) (oh (quote no)))
      ((atom? (car l))
       (if (eq? (car l) a)
           (cdr l)
           (cons (car l)
                 (rm a (cdr l) oh))))
      (else
       (let ((new-car
              (let/cc oh
                (rm a (car l) oh))))
         (if (atom? new-car)
             (cons (car l)
                   (rm a (cdr l) oh))
             (cons new-car (cdr l))))))))

;;; define try in racket
;;; my first macro
(define-syntax try
  (syntax-rules ()
    ((try x α β)
     (let/cc success
       (let/cc x (success α))
       β))))

(define rember5*
  (lambda (a l)
    (try oh (rm3 a l oh) l)))

;;; simplify rm with (try ...)
;;; maybe need to review.
(define rm3
  (lambda (a l oh)
    (cond
      ((null? l) (oh (quote no)))
      ((atom? (car l))
       (if (eq? (car l) a)
           (cdr l)
           (cons (car l)
                 (rm a (cdr l) oh))))
      (else
       (try oh2
            (cons (rm a (car l) oh2)
                  (cdr l))
            (cons (car l)
                  (rm a (cdr l) oh)))))))
