#lang racket
(require "mk.rkt")
;;; The Fun Never Ends...

;;; Here is an unusual definition.
(define anyo
  (lambda (g)
    (conde
      (g succeed)
      (else (anyo g)))))

;;; Here is another definition.
(define nevero
  (anyo fail))

;; (run 1 (q)
;;   nevero
;;   (== #t q))
;; has no value

(run 1 (q)
  fail
  nevero)
; => '()

(define alwayso
  (anyo succeed))

(run 1 (q)
  alwayso
  (== #t q))
; => '(#t)

;; (run* (q)
;;   alwayso
;;   (== #t q))
;;; has no value.

(run 5 (q)
  alwayso
  (== #t q))
; => '(#t #t #t #t #t)

(run 5 (q)
  (== #t q)
  alwayso)
; => '(#t #t #t #t #t)

;;; succeeds at least once.
(define salo
  (lambda (g)
    (conde
      (succeed succeed)
      (else g))))

(run 1 (q)
  (salo alwayso)
  (== #t q))
; => '(#t)

(run 1 (q)
  (salo nevero)
  (== #t q))
; => '(#t)

;; (run* (q)
;;   (salo nevero)
;;   (== #t q))
;;; It has no value.

;; (run 1 (q)
;;   (salo nevero)
;;   fail
;;   (== #t q))
;;; It has no value.

;; (run 1 (q)
;;   alwayso
;;   fail
;;   (== #t q))
;;; It has no value.

(run 1 (q)
  (condi
   ((== #f q) alwayso)
   (else (anyo (== #t q))))
  (== #t q))
; => '(#t)

;; (run 2 (q)
;;   (condi
;;    ((== #f q) alwayso)
;;    (else (== #t q)))
;;   (== #t q))
;;; It has no value.

(run 5 (q)
  (condi
   ((== #f q) alwayso)
   (else (anyo (== #t q))))
  (== #t q))
; => '(#t #t #t #t #t)

;;; The Law of condi
;;; condi behaves like conde, except that its
;;; values are interleaved.

(define teacupo
  (lambda (x)
    (conde
      ((== 'tea x) succeed)
      ((== 'cup x) succeed)
      (else fail))))

(run 5 (r)
  (condi
   ((teacupo r) succeed)
   ((== #f r) succeed)
   (else fail)))
; => '(tea #f cup)

(run 5 (q)
  (condi
   ((== #f q) alwayso)
   ((== #t q) alwayso)
   (else fail))
  (== #t q))
; => '(#t #t #t #t #t)

;; (run 5 (q)
;;   (conde
;;     ((== #f q) alwayso)
;;     ((== #t q) alwayso)
;;     (else fail))
;;   (== #t q))
; => have no value.

(run 5 (q)
  (conde
    (alwayso succeed)
    (else nevero))
  (== #t q))
; => '(#t #t #t #t #t)

;; (run 5 (q)
;;   (condi
;;    (alwayso succeed)
;;    (nevero))
;;   (== #t q))
; => have no value.

;; (run 1 (q)
;;   (all
;;    (conde
;;      ((== #f q) succeed)
;;      (else (== #t q)))
;;    alwayso)
;;   (== #t q))
; => have no value.
; The goals of an all must succeed for the all to succeed.

(run 1 (q)
  (alli
   (conde
     ((== #f q) succeed)
     (else (== #t q)))
   alwayso)
  (== #t q))
; => '(#t)

(run 5 (q)
  (alli
   (conde
     ((== #f q) succeed)
     (else (== #t q)))
   alwayso)
  (== #t q))
; => '(#t #t #t #t #t)

(run 5 (q)
  (alli
   (conde
     ((== #t q) succeed)
     (else (== #f q)))
   alwayso)
  (== #t q))
; => '(#t #t #t #t #t)
; i stand for interleave.

(run 5 (q)
  (all
   (conde
     (succeed succeed)
     (else nevero))
   alwayso)
  (== #t q))
; => '(#t #t #t #t #t)

;; (run 5 (q)
;;   (alli
;;    (conde
;;      (succeed succeed)
;;      (else nevero))
;;    alwayso)
;;   (== #t q))
; have no value.
