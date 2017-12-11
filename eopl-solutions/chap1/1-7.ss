#lang eopl

(require "base.ss")

;;; nth-element : List x Int -> SchemeVal
;;; usage: (nth-element lst n) = the n-th element of lst
;;; helper: h : List x Int -> SchemeVal
;;; usage: h can use nth-element's params as original params.
(define nth-element
  (lambda (lst n)
    (letrec
        ((h (lambda (hl hn)
              (if (null? hl)
                  (report-list-too-short lst n)
                  (if (zero? hn)
                      (car hl)
                      (h (cdr hl) (- hn 1)))))))
      (h lst n))))

;;; report-list-too-short : List x Int -> String
;;; usage: report-list-too-short need original params.
(define report-list-too-short
  (lambda (lst n)
    (eopl:error 'nth-element
                "~s does not have ~s elements. ~%" lst (+ n 1))))

(equal?? (nth-element '(2 3) 0) 2)
(equal?? (nth-element '(2 3) 1) 3)
(equal?? (nth-element '(2 3) 2) 2)
