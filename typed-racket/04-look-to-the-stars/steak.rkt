#lang typed/racket

(define-type meza
  (U Shrimp
     Calamari
     Escargots
     Hummus))

(struct Shrimp () #:transparent)
(struct Calamari () #:transparent)
(struct Escargots () #:transparent)
(struct Hummus () #:transparent)

(define-type main
  (U Steak
     Ravioli
     Chicken
     Eggplant))

(struct Steak () #:transparent)
(struct Ravioli () #:transparent)
(struct Chicken () #:transparent)
(struct Eggplant () #:transparent)

(define-type salad
  (U Green
     Cucumber
     Greek))

(struct Green () #:transparent)
(struct Cucumber () #:transparent)
(struct Greek () #:transparent)

(define-type desserts
  (U Sundae
     Mousse
     Torte))

(struct Sundae () #:transparent)
(struct Mousse () #:transparent)
(struct Torte () #:transparent)

;; (: add-a-steak (-> meza (Pairof meza main)))
;; we don't need a type denotation here.
(define (add-a-steak m)
  (match m
    [(Shrimp)
     (cons (Shrimp)
           (Steak))]
    [(Calamari)
     (cons (Calamari)
           (Steak))]
    [(Escargots)
     (cons (Escargots)
           (Steak))]
    [(Hummus)
     (cons (Hummus)
           (Steak))]))

(: add-a-steak2
   (-> meza (Pairof meza main)))
(define add-a-steak2
  (lambda (m)
    (cons m (Steak))))

;;; I'm confused on how to use pattern match here.
;; (: eq-main (-> main main Boolean))
;; (define eq-main
;;   (lambda (t m)
;;     (cond
;;       [(and (Steak? t) (Steak? m)) true]
;;       [(and (Ravioli? t) (Ravioli? m)) true]
;;       [(and (Chicken? t) (Chicken? m)) true]
;;       [(and (Eggplant? t) (Eggplant? m)) true]
;;       [else false])))

;;; ok, I find it.
;; (: eq-main (-> main main Boolean))
;; (define (eq-main t m)
;;   (match t
;;     [(Steak)
;;      (Steak? m)]
;;     [(Ravioli)
;;      (Ravioli? m)]
;;     [(Chicken)
;;      (Chicken? m)]
;;     [(Eggplant)
;;      (Eggplant? m)]
;;     [_ false]))

;; a better way
(: eq-main (-> main main Boolean))
(define (eq-main t m)
  (match* (t m)
    [((Steak) (Steak))
     true]
    [((Ravioli) (Ravioli))
     true]
    [((Chicken) (Chicken))
     true]
    [((Eggplant) (Eggplant))
     true]
    [(_ _) false]))


(: has-steak (-> meza
                 Any
                 desserts
                 Boolean))
(define (has-steak a b c)
  (match b
    [(Steak) true]
    [_ false]))
