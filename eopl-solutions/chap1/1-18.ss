#lang eopl

(require "base.ss")

;;; swapper : Sym x Sym x slist -> slist
;;; usage: (swapper s1 s2 slist) returns a lisp the same as slist,
;;;        but with all occurrences of s1 replaced by s2 and all
;;;        occurrences of s2 replaced by s1.
(define swapper
  (lambda (s1 s2 slist)
    (cond
      ((null? slist) '())
      ((atom? (car slist))
       (cond
         ((eq? (car slist) s1)
          (cons s2 (swapper s1 s2 (cdr slist))))
         ((eq? (car slist) s2)
          (cons s1 (swapper s1 s2 (cdr slist))))
         (else
          (cons (car slist) (swapper s1 s2 (cdr slist))))))
      (else
       (cons (swapper s1 s2 (car slist))
             (swapper s1 s2 (cdr slist)))))))

(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

(define swapper-b
  (lambda (s1 s2 slist)
    (letrec
        ((S (lambda (sexp)
              (cond
                ((not (atom? sexp))
                 (swapper-b s1 s2 sexp))
                ((eq? s1 sexp) s2)
                ((eq? s2 sexp) s1)
                (else sexp))))
         (atom? (lambda (a)
                  (and (not (pair? a))
                       (not (null? a))))))
      (map S slist))))

(equal?? (swapper 'a 'd '(a b c d))
         '(d b c a))

(equal?? (swapper 'a 'd '(a d () c d))
         '(d a () c a))

(equal?? (swapper 'x 'y '((x) y (z (x))))
         '((y) x (z (y))))

(equal?? (swapper-b 'a 'd '(a b c d))
         '(d b c a))

(equal?? (swapper-b 'a 'd '(a d () c d))
         '(d a () c a))

(equal?? (swapper-b 'x 'y '((x) y (z (x))))
         '((y) x (z (y))))
