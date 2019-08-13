#lang racket/base

;; need fix

(define atom?
  (lambda (a)
    (and (not (null? a)) (not (pair? a)))))

(define (build s1 s2)
  (cons s1
        (cons s2 '())))

;; How can we build an entry from a set of names and
;; a list of values?
(define new-entry build)
(new-entry 1 2)

(define first car)
(define second cdr)

(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
     ((null? names) (entry-f name))
     ((eq? (car names) name)
      (car values))
     (else (lookup-in-entry-help name
                                 (cdr names)
                                 (cdr values)
                                 entry-f)))))

;; define the function extend-table which takes
;; an entry and a table (possibly the empty one)
;; and creates a new table by putting the new entry
;; in front of the old table.
(define extend-table cons)

(define lookup-in-table
  (lambda (name table table-f)
    (cond
     ((null? table) (table-f name))
     (else (lookup-in-entry name
                            (car table)
                            (lambda (name)
                              (lookup-in-table name
                                               (cdr table)
                                               table-f)))))))

(define expression-to-action
  (lambda (e)
    (cond
     ((atom? e) (atom-to-action e))
     (else (list-to-action e)))))

(define atom-to-action
  (lambda (e)
    (cond
     ((number? e) *const)
     ((eq? e #t) *const)
     ((eq? e #f) *const)
     ((eq? e (quote cons)) *const)
     ((eq? e (quote car)) *const)
     ((eq? e (quote null?)) *const)
     ((eq? e (quote eq?)) *const)
     ((eq? e (quote atom?)) *const)
     ((eq? e (quote zero?)) *const)
     ((eq? e (quote add1)) *const)
     ((eq? e (quote sub1)) *const)
     ((eq? e (quote number?)) *const)
     (else *identifier))))

(define list-to-action
  (lambda (e)
    (cond
     ((atom? (car e))
      (cond
       ((eq? (car e) (quote quote))
        *quote)
       ((eq? (car e) (quote lambda))
        *lambda)
       ((eq? (car e) (quote cond))
        *cond)
       (else *application)))
     (else *application))))

(define value
  (lambda (e)
    (meaning e (quote ()))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
     ((number? e) e)
     ((eq? e #t) #t)
     ((eq? e #f) #f)
     (else (build (quote primitive) e)))))

(define *quote
  (lambda (e table)
    (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

;; raise an empty list error
(define initial-table
  (lambda (name)
    (car (quote ()))))

(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))
;; (table formals body) is called a closure record.

(define table-of first)
(define formals-of second)
(define body-of (lambda (lst) (list-ref lst 2)))

(define evcon
  (lambda (lines table)
    (cond
     ((else? (question-of (car lines)))
      (meaning (answer-of (car lines))
               table))
     ((meaning (question-of (car lines))
               table)
      (meaning (answer-of (car lines))
               table))
     (else (evcon (cdr lines) table)))))

(define else?
  (lambda (x)
    (cond
     ((atom? x) (eq? x (quote else)))
     (else #f))))

(define question-of first)
(define answer-of second)

(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))

(define cond-lines-of cdr)

(*cond '(cond (coffee klatsch) (else party))
       '(((coffee) (#t))
         ((klatsch party) (5 6))))

(define evlis
  (lambda (args table)
    (cond
     ((null? args) (quote ()))
     (else
      (cons (meaning (car args) table)
            (evlis (cdr args) table))))))

(define *application
  (lambda (e table)
    (my-apply
     (meaning (function-of e) table)
     (evlis (arguments-of e) table))))

(define function-of car)
(define arguments-of cdr)

(define my-primitive?
  (lambda (l)
    (eq? (first l) (quote primitive))))

(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))

(define my-apply
  (lambda (fun vals)
    (cond
     ((my-primitive? fun)
      (apply-primitive
       (second fun) vals))
     ((non-primitive? fun)
      (apply-closure
       (second fun) vals)))))

(define apply-primitive
  (lambda (name vals)
    (cond
     ((eq? name (quote cons))
      (cons (first vals) (second vals)))
     ((eq? name (quote car))
      (car (first vals)))
     ((eq? name (quote cdr))
      (cdr (first vals)))
     ((eq? name (quote null?))
      (null? (first vals)))
     ((eq? name (quote eq?))
      (eq? (first vals) (second vals)))
     ((eq? name (quote atom?))
      (:atom? (first vals)))
     ((eq? name (quote zero?))
      (zero? (first vals)))
     ((eq? name (quote add1))
      (add1 (first vals)))
     ((eq? name (quote sub1))
      (sub1 (first vals)))
     ((eq? name (quote number?))
      (number? (first vals))))))

(define :atom?
  (lambda (a)
    (cond
     ((atom? a) #t)
     ((null? a) #f)
     ((eq? (car a) (quote primitive))
      #t)
     ((eq? (car a) (quote non-primitive))
      #t)
     (else #f))))

(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
              (new-entry (formals-of closure)
                         vals)
              (table-of closure)))))

(apply-closure '((((u v w)
                   (1 2 3))
                  ((x y z)
                   (4 5 6)))
                 (x y)
                 (cons z y))
               '((a b c) (d e f)))

;; meaning *application then use my-apply
(meaning (body-of '((((u v w)
                      (1 2 3))
                     ((x y z)
                      (4 5 6)))
                    (x y)
                    (cons z y)))
         (extend-table (new-entry '(x y)
                                  '((a b c) (d e f)))
                       '(((u v w)
                          (1 2 3))
                         ((x y z)
                          (4 5 6)))))

;; but what about (define ...)
;; it isn't needed because recursion can be obtained from
;; the Y combinator
;; we will see this question in The Seasoned Schemer.
