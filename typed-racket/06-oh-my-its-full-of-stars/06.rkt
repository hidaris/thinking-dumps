#lang typed/racket

(define-type Bool Boolean)
(define-type Int Integer)

(define-type fruit
  (U Peach Apple
     Pear Lemon
     Fig))

(struct Peach ()
  #:transparent)
(struct Apple ()
  #:transparent)
(struct Pear ()
  #:transparent)
(struct Lemon ()
  #:transparent)
(struct Fig ()
  #:transparent)

(define-type tree
  (U Bud
     Flat
     Split))

(struct Bud ()
  #:transparent)
(struct Flat ([f : fruit] [t : tree])
  #:transparent)
(struct Split ([s : tree] [t : tree])
  #:transparent)

(: flat-only : (tree -> Bool))
(define (flat-only tree)
  (match tree
    [(Bud) true]
    [(Flat f t) (flat-only t)]
    [(Split s t) false]))

(: split-only : (tree -> Bool))
(define (split-only tree)
  (match tree
    [(Bud) true]
    [(Flat f t) false]
    [(Split s t)
     (if (split-only s)
         (split-only t)
         false)]))

(: contains-fruit : (tree -> Bool))
(define (contains-fruit tree)
  (match tree
    [(Bud) false]
    [(Flat f t) true]
    [(Split s t)
     (if (contains-fruit s)
         true
         (contains-fruit t))]))

(: less-than (-> Int Int
                 Bool))
(define (less-than n m)
  (< n m))

(: larger-of (-> Int Int
                 Int))
(define (larger-of n m)
  (if (less-than n m)
      m
      n))

(: height : (tree -> Int))
(define (height tree)
  (match tree
    [(Bud) 0]
    [(Flat f t) (+ 1 (height t))]
    [(Split s t)
     (+ 1 (larger-of (height s)
                     (height t)))]))

(: eq-fruit : (fruit fruit -> Bool))
(define (eq-fruit f1 f2)
  (match* (f1 f2)
    [((Peach) (Peach)) true]
    [((Apple) (Apple)) true]
    [((Pear) (Pear)) true]
    [((Lemon) (Lemon)) true]
    [((Fig) (Fig)) true]
    [(_ _) false]))

(: subst-in-tree
   (-> fruit fruit tree
       tree))
(define (subst-in-tree n a tree)
  (match tree
    [(Bud) (Bud)]
    [(Flat f t)
     (if (eq-fruit f a)
         (Flat n (subst-in-tree n a t))
         (Flat f (subst-in-tree n a t)))]
    [(Split s t)
     (Split (subst-in-tree n a s)
            (subst-in-tree n a t))]))

(: occurs (-> fruit tree
              Int))
(define (occurs a tree)
  (match tree
    [(Bud) 0]
    [(Flat f t)
     (if (eq-fruit f a)
         (+ 1 (occurs a t))
         (occurs a t))]
    [(Split s t)
     (+ (occurs a s)
        (occurs a t))]))

;;; the first definition refers to the second
;;; and the second refers to the first
(define-type (slist a)
  (U Empty (Scons a)))

(struct Empty ())
(struct (a) Scons ([v : (sexp a)] [w : (slist a)]))

(define-type (sexp a)
  (U (An-atom a)
     (A-slist a)))

(struct (a) An-atom ([v : a]))
(struct (a) A-slist ([v : (slist a)]))

(: occurs-in-slist : fruit (slist fruit) -> Int)
(define (occurs-in-slist a slist)
  (match slist
    [(Empty) 0]
    [(Scons s y)
     (letrec ([occurs-in-sexp
               : (-> fruit (sexp fruit)
                     Int)
               (λ (a sexp)
                 (match sexp
                   [(An-atom b)
                    (if (eq-fruit b a)
                        1
                        0)]
                   [(A-slist y)
                    (occurs-in-slist a y)]))])
       (+ (occurs-in-sexp a s)
          (occurs-in-slist a y)))]))

(: subst-in-slist (-> fruit fruit (slist fruit)
                      (slist fruit)))
(define (subst-in-slist n a slist)
  (match slist
    [(Empty) (Empty)]
    [(Scons s y)
     (letrec ([subst-in-sexp
               : (-> fruit fruit (sexp fruit)
                     (sexp fruit))
               (lambda (n a sexp)
                 (match sexp
                   [(An-atom b)
                    (if (eq-fruit b a)
                        (An-atom n)
                        (An-atom b))]
                   [(A-slist y)
                    (A-slist (subst-in-slist n a y))]))])
       (Scons (subst-in-sexp n a s)
              (subst-in-slist n a y)))]))

(: rem-from-slist (-> fruit (slist fruit)
                      (slist fruit)))
(define (rem-from-slist a slist)
  (match slist
    [(Empty) (Empty)]
    [(Scons s y)
     (letrec ([rem-from-sexp
               : (-> fruit (sexp fruit)
                     (sexp fruit))
               (λ (a sexp)
                 (match sexp
                   [(An-atom b) (An-atom b)]
                   [(A-slist y)
                    (A-slist (rem-from-slist a y))]))]
              [eq-fruit-in-atom
               : (-> fruit (sexp fruit)
                     Bool)
               (λ (a sexp)
                 (match sexp
                   [(An-atom s) (eq-fruit a s)]
                   [(A-slist y) false]))])
       (if (eq-fruit-in-atom a s)
           (rem-from-slist a y)
           (Scons (rem-from-sexp a s)
                  (rem-from-slist a y))))]))
