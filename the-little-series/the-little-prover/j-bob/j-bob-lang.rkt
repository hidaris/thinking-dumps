#lang racket/base

(provide
   (rename-out [j.car car])
   (rename-out [j.cdr cdr])
   (rename-out [j.+ +])
   (rename-out [j.< <])
   (rename-out [j.if if])
   (rename-out [atom atom])
   (rename-out [num num])
   (rename-out [equal equal])
   (rename-out [natp natp])
   (rename-out [defun defun])
   (rename-out [dethm dethm])
   (rename-out [size size])
   all-defined-out)

(define (if/nil Q A E)
  (if (equal? Q 'nil) (E) (A)))

(define (num x) (if (number? x) x 0))
(define (atom x) (if (pair? x) 'nil 't))
(define (j.car x) (if (pair? x) (car x) '()))
(define (j.cdr x) (if (pair? x) (cdr x) '()))
(define (equal x y) (if (equal? x y) 't 'nil))

(define (natp x)
  (if (integer? x)
      (if (< x 0) 'nil 't)
      'nil))

(define (j.+ x y) (+ (num x) (num y)))

(define (j.< x y)
  (if (< (num x) (num y)) 't 'nil))

(define-syntax j.if
  (syntax-rules ()
    ((_ Q A E)
     (if/nil Q (lambda () A) (lambda () E)))))

(define-syntax defun
  (syntax-rules ()
    ((_ name (arg ...) body)
     (define (name arg ...) body))))

(define-syntax dethm
  (syntax-rules ()
    ((_ name (arg ...) body)
     (define (name arg ...) body))))

(defun size (x)
  (if (atom x)
    '0
    (j.+ '1 (j.+ (size (j.car x)) (size (j.cdr x))))))
