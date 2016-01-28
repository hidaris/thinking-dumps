#lang racket
(require "mk.rkt")

;;; Seeing Old Friends in New Ways

(define list?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((pair? l) (list? (cdr l)))
      (else #f))))

(list? '((a) (a b) c))
; => #t

(list? '())
; => #t

(list? 's)
; => #f

(list? '(d a t e . s))
; => #f

;;; Consider the definition of listo
(define listo
  (lambda (l)
    (conde
      ((nullo l) succeed)
      ((pairo l)
       (fresh (d)
         (cdro l d)
         (listo d)))
      (else fail))))

;;; The First Commandment
;;; To transform a function whose value is a Boolean
;;; into a function whose value is goal, replace cond
;;; with conde and unnest each question and answer.
;;; Unnest the answer #t (or #f) by replacing it with
;;; succeed (or fail).

(run* (x)
  (listo `(a b ,x d)))
; => '(_.0)

(run 1 (x)
  (listo `(a b c . ,x)))
; => '(())

(run 5 (x)
  (listo `(a b c . ,x)))
; => '(()
;      (_.0)
;      (_.0 _.1)
;      (_.0 _.1 _.2)
;      (_.0 _.1 _.2 _.3))

;;; Consider the definition of lol?, where lol?
;;; stands for list-of-lists?.
(define lol?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((list? (car l)) (lol? (cdr l)))
      (else #f))))

;;; Here is the definition of lolo
(define lolo
  (lambda (l)
    (conde
      ((nullo l) succeed)
      ((fresh (a)
         (caro l a)
         (listo a))
       (fresh (d)
         (cdro l d)
         (lolo d)))
      (else fail))))

(run 1 (l)
  (lolo l))
; => '(())

(run* (q)
  (fresh (x y)
    (lolo `((a b) (,x c) (d ,y)))
    (== #t q)))
; => '(#t)

(run 1 (q)
  (fresh (x)
    (lolo `((a b) . ,x))
    (== #t q)))
; => '(#t) nullo of a fresh variable always succeeds,
;;;        and associates the fresh varible with '()

(run 1 (x)
  (lolo `((a b) (c d) . ,x)))
; => '(())

(run 5 (x)
  (lolo `((a b) (c d) . ,x)))
; => '(()
;      (())
;      (() ())
;      (() () ())
;      (() () () ()))

;; Consider the definition of twinso.
;; (define twinso
;;   (lambda (s)
;;     (fresh (x y)
;;       (conso x y s)
;;       (conso x '() y))))

;;; Redefine twinso without using conso
(define twinso
  (lambda (s)
    (fresh (x)
      (== `(,x ,x) s))))

(run* (q)
  (twinso '(tofu tofu))
  (== #t q))
; => '(#t)

(run* (z)
  (twinso `(,z tofu)))
; => '(tofu)

;;; Consider the definition of loto.
;;; lot stands for list-of-twins.
(define loto
  (lambda (l)
    (conde
      ((nullo l) succeed)
      ((fresh (a)
         (caro l a)
         (twinso a))
       (fresh (d)
         (cdro l d)
         (loto d)))
      (else fail))))

(run 1 (z)
  (loto `((g g) . ,z)))
; => '(())

(run 5 (z)
  (loto `((g g) . ,z)))
;; => '(()
;;     ((_.0 _.0))
;;     ((_.0 _.0) (_.1 _.1))
;;     ((_.0 _.0) (_.1 _.1) (_.2 _.2))
;;     ((_.0 _.0) (_.1 _.1) (_.2 _.2) (_.3 _.3)))

(run 5 (r)
  (fresh (w x y z)
    (loto `((g g) (e ,w) (,x ,y) . ,z))
    (== `(,w (,x ,y) ,z) r)))
; => '((e (_.0 _.0) ())
;     (e (_.0 _.0) ((_.1 _.1)))
;     (e (_.0 _.0) ((_.1 _.1) (_.2 _.2)))
;     (e (_.0 _.0) ((_.1 _.1) (_.2 _.2) (_.3 _.3)))
;     (e (_.0 _.0) ((_.1 _.1) (_.2 _.2) (_.3 _.3) (_.4 _.4))))

(run 3 (out)
  (fresh (w x y z)
    (== `((g g) (e ,w) (,x ,y) . ,z) out)
    (loto out)))
; => '(((g g) (e e) (_.0 _.0))
;      ((g g) (e e) (_.0 _.0) (_.1 _.1))
;      ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2)))

;;; Here is listofo
(define listofo
  (lambda (predo l)
    (conde
      ((nullo l) succeed)
      ((fresh (a)
         (caro l a)
         (predo a))
       (fresh (d)
         (cdro l d)
         (listofo predo d)))
      (else fail))))

(run 3 (out)
  (fresh (w x y z)
    (== `((g g) (e ,w) (,x ,y) . ,z) out)
    (listofo twinso out)))
; => '(((g g) (e e) (_.0 _.0))
;      ((g g) (e e) (_.0 _.0) (_.1 _.1))
;      ((g g) (e e) (_.0 _.0) (_.1 _.1) (_.2 _.2)))

;;; Redefine loto using listofo and twinso
;; (define loto
;;   (lambda (l)
;;     (listofo twinso l)))

;;; Remember member?
(define member?
  (lambda (x l)
    (cond
      ((null? l) #f)
      ((eq-car? l x) #t)
      (else (member? x (cdr l))))))

(define eq-car?
  (lambda (l x)
    (eq? (car l) x)))

(member? 'olive '(virgin olive oil))
; => #t

;;; Consider this definition of eq-caro
(define eq-caro
  (lambda (l x)
    (caro l x)))

;;; Define membero
(define membero
  (lambda (x l)
    (conde
      ((nullo l) fail)
      ((eq-caro l x) succeed)
      (else
       (fresh (d)
         (cdro l d)
         (membero x d))))))

(run* (q)
  (membero 'olive '(virgin olive oil))
  (== #t q))
; => '(#t)

(run 1 (y)
  (membero y '(hummus with pita)))
; => '(hummus)

(run 1 (y)
  (membero y '(with pita)))
; => '(with)

(run 1 (y)
  (membero y '(pita)))
; => '(pita)

(run 1 (y)
  (membero y '()))
; => '()

(run* (y)
  (membero y '(hummus with pita)))
; => '(hummus with pita)

(run* (y)
  (membero y '(a b c)))
; => '(a b c)

;;; identity, whose argument is a list, and which returns that list.
(define my-identity
  (lambda (l)
    (run* (y)
      (membero y l))))

(run* (x)
  (membero 'e `(pasta ,x fagioli)))
; => '(e)

(run 1 (x)
  (membero 'e `(pasta e ,x fagioli)))
; => '(_.0)

(run 1 (x)
  (membero 'e `(pasta ,x e fagioli)))
; => '(e)

(run* (r)
  (fresh (x y)
    (membero 'e `(pasta ,x fagioli ,y))
    (== `(,x ,y) r)))
; => '((e _.0) (_.0 e))

(run 1 (l)
  (membero 'tofu l))
; => '((tofu . _.0))

(run 5 (l)
  (membero 'tofu l))
; => '((tofu . _.0)
;      (_.0 tofu . _.1)
;      (_.0 _.1 tofu . _.2)
;      (_.0 _.1 _.2 tofu . _.3)
;      (_.0 _.1 _.2 _.3 tofu . _.4))

;;; Here is a definition of pmembero
(define pmembero
  (lambda (x l)
    (conde
      ((nullo l) fail)
      ((eq-caro l x) (cdro l '()))
      ;; ((eq-caro l x) succeed)
      (else
       (fresh (d)
         (cdro l d)
         (pmembero x d))))))

(run 5 (l)
  (pmembero 'tofu l))
; => '((tofu)
;      (_.0 tofu)
;      (_.0 _.1 tofu)
;      (_.0 _.1 _.2 tofu)
;      (_.0 _.1 _.2 _.3 tofu))

(run* (q)
  (pmembero 'tofu '(a b tofu d tofu))
  (== #t q))
; => '(#t)

(define pmembero2
  (lambda (x l)
    (conde
      ((nullo l) fail)
      ((eq-caro l x) (cdro l '()))
      ((eq-caro l x) succeed)
      ((fresh (d)
         (cdro l d)
         (pmembero2 x d))))))

(run* (q)
  (pmembero2 'tofu '(a b tofu d tofu))
  (== #t q))
; => '(#t #t #t)

(define pmemberof
  (lambda (x l)
    (conde
      ((nullo l) fail)
      ((eq-caro l x) (cdro l '()))
      ((eq-caro l x)
       (fresh (a d)
         (cdro l `(,a . ,d))))
      ((fresh (d)
         (cdro l d)
         (pmemberof x d))))))

(run* (q)
  (pmemberof 'tofu '(a b tofu d tofu))
  (== #t q))
; => '(#t #t)

(run 12 (l)
  (pmemberof 'tofu l))
; => '((tofu)
;      (tofu _.0 . _.1)
;      (_.0 tofu)
;      (_.0 tofu _.1 . _.2)
;      (_.0 _.1 tofu)
;      (_.0 _.1 tofu _.2 . _.3)
;      (_.0 _.1 _.2 tofu)
;      (_.0 _.1 _.2 tofu _.3 . _.4)
;      (_.0 _.1 _.2 _.3 tofu)
;      (_.0 _.1 _.2 _.3 tofu _.4 . _.5)
;      (_.0 _.1 _.2 _.3 _.4 tofu)
;      (_.0 _.1 _.2 _.3 _.4 tofu _.5 . _.6))

;;; redefine pmembero so that the lists in the odd even position are
;;; swapped
(define pmemberos
  (lambda (x l)
    (conde
      ((nullo l) fail)
      ((eq-caro l x)
       (fresh (a d)
         (cdro l `(,a . ,d))))
      ((eq-caro l x) (cdro l '()))
      (else
       (fresh (d)
         (cdro l d)
         (pmemberos x d))))))

(run 12 (l)
  (pmemberos 'tofu l))
; => '((tofu _.0 . _.1)
;      (tofu)
;      (_.0 tofu _.1 . _.2)
;      (_.0 tofu)
;      (_.0 _.1 tofu _.2 . _.3)
;      (_.0 _.1 tofu)
;      (_.0 _.1 _.2 tofu _.3 . _.4)
;      (_.0 _.1 _.2 tofu)
;      (_.0 _.1 _.2 _.3 tofu _.4 . _.5)
;      (_.0 _.1 _.2 _.3 tofu)
;      (_.0 _.1 _.2 _.3 _.4 tofu _.5 . _.6)
;      (_.0 _.1 _.2 _.3 _.4 tofu))


;;; first-value : list -> val
;;; usage: (first-value l) takes a list of values l and returns a list
;;;        that contains the first value in l.
(define first-value
  (lambda (l)
    (run 1 (y)
      (membero y l))))

(first-value '(pasta e fagioli))
; => '(pasta)

;;; Consider this variant of membero
(define memberrevo
  (lambda (x l)
    (conde
      ((nullo l) fail)
      (succeed
       (fresh (d)
         (cdro l d)
         (memberrevo x d)))
      (else (eq-caro l x)))))

(run* (x)
  (memberrevo x `(pasta e fagioli)))
; => '(fagioli e pasta)
; => '(pasta e fagioli) ; cKanren.
;;; in cKanren, the result is strange.

(define reverse-list
  (lambda (l)
    (run* (y)
      (memberrevo y l))))

(reverse-list '(a b c))
; => '(c b a)
