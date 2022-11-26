#lang racket

(require "mk.rkt")

(define assoco
  (lambda (x ls out)
    (fresh (a d aa da)
      (== `(,a . ,d) ls)
      (== `(,aa . ,da) a)
      (conde
       ((== aa x) (== a out))
       ((=/= aa x) (assoco x d out))))))

(define (appendo l t out)
  (conde
   [(== '() l) (== t out)]
   [(fresh (a d res)
      (== `(,a . ,d) l)
      (== `(,a . ,res) out)
      (appendo d t res))]))


(define reverseo
  (lambda (ls out)
    (conde
     [(== '() ls) (== '() out)]
     [(fresh (a d res)
       (== `(,a . ,d) ls)
       (appendo res `(,a) out)
       (reverseo d res))])))

(define stuttero
  (lambda (ls out)
    (conde
     ((== '() ls) (== '() out))
     ((fresh (a d res)
        (== `(,a . ,d) ls)
        (== `(,a ,a . ,res) out)
        (stuttero d res))))))

(require "numbers.rkt")

(define (lengtho l n)
  (conde
   [(== '() l) (== '() n)]
   [(fresh (d res)
      (cdro l d)
      (+o '(1) res n)
      (lengtho d res))]))
