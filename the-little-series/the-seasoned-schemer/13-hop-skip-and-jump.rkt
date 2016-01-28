#lang racket/base

;; (intersect '(a b c) '(b c d)) -> '(b c)
(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) (quote ()))
      ((member? (car set1) set2)
       (cons (car set1)
             (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? (car lat) a) #t)
      (else (member? a (cdr lat))))))

;; rewrite by the twelfth commandment
(define intersect-letrec
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set)
              (cond
                ((null? set) (quote ()))
                ((member? (car set) set2)
                 (cons (car set)
                       (I (cdr set))))
                (else (I (cdr set)))))))
      (I set1))))

;; intersects a list of sets
(define intersectall
  (lambda (lset)
    (cond
      ((null? (cdr lset)) (car lset))
      (else (intersect (car lset)
                       (intersectall (cdr lset)))))))

;; why don't we ask (null? lset)
;; there is no need. since we assumes that
;; the list of sets for intersectall is not empty.

;; a version of intersectall that
;; makes no assumptions about the
;; list of sets.
(define intersectall2
  (lambda (lset)
    (cond
      ((null? lset) (quote ()))
      ((null? (cdr lset)) (car lset))
      (else (intersect (car lset)
                       (intersectall2
                        (cdr lset)))))))
;; (intersectall2 '((a b c) (b d e) (d b a))) ; '()
;; why? when (cdr lset) is empty, program doesn't recur
;; what should we do then?
;; ask the question once and use the old version
;; of intersectall if the list is not empty.
(define intersectall-letrec
  (lambda (lset)
    (letrec
        ((intersectall
          (lambda (lset)
            (cond
              ((null? (cdr lset)) (car lset))
              (else (intersect (car lset)
                               (intersectall
                                (cdr lset))))))))
      (cond
        ((null? lset) (quote ()))
        (else (intersectall lset))))))

;; intersectall is just a better name,
;; though a bit long for these boxes.
;; we can use whatever name we want for
;; such a minor function if nobody else
;; relies on it. because (letrec ...) hides
;; definitions, and the name matter only inside
;; of (letrec ...).
;; this is similar to (lambda (x y) M), the names
;; x and y matter only inside of M, whatever M is.
;; And in (letrec ((x F) (y G)) M) the names x and y
;; matter only inside of F, G, and M, whatever F, G,
;; and M are.
(define intersectall-letrec2
  (lambda (lset)
    (letrec
        ((A (lambda (lset)
              (cond
                ((null? (cdr lset)) (car lset))
                (else (intersect (car lset)
                                 (A (cdr lset))))))))
      (cond
        ((null? lset) (quote ()))
        (else (A lset))))))

(define intersectall-letcc
  (lambda (lset)
    (let/cc hop
      (letrec
          ((A (lambda (lset)
                (cond
                  ((null? (car lset))
                   (hop (quote ())))
                  ((null? (cdr lset))
                   (car lset))
                  (else
                   (intersect (car lset)
                              (A (cdr lset))))))))
        (cond
          ((null? lset) (quote ()))
          (else (A lset)))))))
;; the fourteenth commandment
;; use (let/cc ...) to return values abruptly and promptly.

;; avoid duplicate when we know the set2 is empty.
;; but we still duplicate ((null? set2) (quote ()))
(define intersect-final
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set)
              (cond
                ((null? set) (quote ()))
                ((member? (car set) set2)
                 (cons (car set)
                       (I (cdr set))))
                (else (I (cdr set)))))))
      (cond
        ((null? set2) (quote ()))
        (else (I set1))))))

;; write a minor function I for intersect-final
;; and when function I meet '() in place s2, we use let/cc to return.
(define intersectall-final
  (lambda (lset)
    (let/cc hop
      (letrec
          ((A (lambda (lset)
                (cond
                  ((null? (car lset))
                   (hop (quote ())))
                  ((null? (cdr lset))
                   (car lset))
                  (else (I (car lset)
                           (A (cdr lset)))))))
           (I (lambda (s1 s2)
                (letrec
                    ((J (lambda (s1)
                          (cond
                            ((null? s1) (quote ()))
                            ((member? (car s1) s2)
                             (cons (car s1)
                                   (J (cdr s1))))
                            (else (J (cdr s1)))))))
                  (cond
                    ((null? s2) (hop (quote ())))
                    (else (J s1)))))))
        (cond
          ((null? lset) (quote ()))
          (else (A lset)))))))

;; rewrite rember by letrec
(define rember
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) (quote ()))
                ((eq? (car lat) a) (cdr lat))
                (else (cons (car lat)
                            (R (cdr lat))))))))
      (R lat))))

(define rember-beyond-first
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) (quote ()))
                ((eq? (car lat) a)
                 (quote ()))
                (else (cons (car lat)
                            (R (cdr lat))))))))
      (R lat))))

(define rember-upto-last
  (lambda (a lat)
    (let/cc skip
      (letrec
          ((R (lambda (lat)
                (cond
                  ((null? lat) (quote ()))
                  ((eq? (car lat) a)
                   (skip (R (cdr lat))))
                  (else
                   (cons (car lat)
                         (R (cdr lat))))))))
        (R lat)))))
