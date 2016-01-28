#lang typed/racket

(define-type number (U Integer numC))
(struct numC ([pred : Integer] [now : Integer]))

(define (Zero) (numC 0 0))
(: One-more-than (Integer -> number))
(define (One-more-than n)
  (numC n (add1 n)))

(struct Too-small exn:fail:user ())

(: succ : (case-> (Integer -> number)
                  (number -> number)
                  ))
(define (succ n)
  (One-more-than n))

;; (: pred : number -> number)
;; (define (pred n)
;;   (match n
;;     [(Zero) (raise
;;              (Too-small
;;               "Too-small"
;;               (current-continuation-marks)))]
;;     [(One-more-than m) m]))

;; (define (is-zero n)
;;   (match n
;;     [(Zero) true]
;;     [_ false]))

;; (: pred : number -> number)
;; (define (pred n)
;;   (if (eq-int n 0)
;;       (raise
;;        (Too-small
;;         "Too-small"
;;         (current-continuation-marks)))
;;       (- n 1)))

;; (: plus (-> number
;;             number
;;             number))
;; (define (plus n m)
;;   (if (is-zero n)
;;       m
;;       (succ (plus (pred n)
;;                   m))))
