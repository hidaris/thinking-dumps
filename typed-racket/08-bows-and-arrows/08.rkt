#lang typed/racket

(define-type Int Integer)
(define-type Bool Boolean)

(define-type (listt a)
  (U Empty (Cons a)))

(struct Empty ()
  #:transparent)
(struct (a) Cons
  ([v : a] [w : (listt a)])
  #:transparent)

(define-type orapl
  (U Orange
     Apple))

(struct Orange () #:transparent)
(struct Apple () #:transparent)

(define (eq-orapl o1 o2)
  (match* (o1 o2)
    [((Orange) (Orange))
     true]
    [((Apple) (Apple))
     true]
    [(_ _) false]))

(: eq-int : Int Int -> Bool)
(define (eq-int n1 n2) (= n1 n2))

(: subst-int (-> Int Int (listt Int)
                 (listt Int)))
(define (subst-int n a listt)
  (match listt
    [(Empty) (Empty)]
    [(Cons e t)
     (if (eq-int a e)
         (Cons n
               (subst-int n a t))
         (Cons e
               (subst-int n a t)))]))

(: subst-orapl (-> orapl orapl (listt orapl)
                   (listt orapl)))
(define (subst-orapl n a listt)
  (match listt
    [(Empty) (Empty)]
    [(Cons e t)
     (if (eq-orapl a e)
         (Cons n
               (subst-orapl n a t))
         (Cons e
               (subst-orapl n a t)))]))

;;; I'm confused about this.
;;; ok, I understand.
(: subst (∀ (a b)
            (-> (-> a b Bool) b a (listt b)
                (listt b))))
(define (subst rel n a listt)
  (match listt
    [(Empty) (Empty)]
    [(Cons e t)
     (if (rel a e)
         (Cons n
               (subst rel n a t))
         (Cons e
               (subst rel n a t)))]))

(: less-than (-> Int Int
                 Bool))
(define (less-than a1 a2) (< a1 a2))

(: in-range-m (-> Int Int Int
                  Bool))
(define (in-range-m small large x)
  (if (less-than small x)
      (less-than x large)
      false))

(: subst-pred (∀ (a)
                 (-> (-> a Bool) a (listt a)
                     (listt a))))
(define (subst-pred pred n listt)
  (match listt
    [(Empty) (Empty)]
    [(Cons e t)
     (if (pred e)
         (Cons n
               (subst-pred pred n t))
         (Cons e
               (subst-pred pred n t)))]))

(: is-15 (-> Int Bool))
(define (is-15 n)
  (eq-int n 15))

(: less-than-15 (-> Int Bool))
(define (less-than-15 x)
  (less-than x 15))

(: in-range-11-16 (-> Int Bool))
(define (in-range-11-16 x)
  (in-range-m 11 16 x))

(: in-range-c : Int Int -> Int -> Bool)
(define (in-range-c small large)
  (lambda (x)
    (if (less-than small x)
        (less-than x large)
        false)))

;;; according Takikawa, it might be a bug/limitation with TR's type inference.
(: subst-c (∀ (a)
              (-> (-> a Bool)
                  (-> a (listt a)
                      (listt a)))))
(define (subst-c pred)
  (lambda (n listt)
    (match listt
      [(Empty) (Empty)]
      [(Cons e t)
       (if (pred e)
           (Cons n (((inst subst-c a) pred) n t))
           (Cons e (((inst subst-c a) pred) n t)))])))

(: subst-c-in-range-11-16 : (Int (listt Int) -> (listt Int)))
(define (subst-c-in-range-11-16 n listt)
  (match listt
    [(Empty) (Empty)]
    [(Cons e t)
     (if (in-range-11-16 e)
         (Cons n
               (subst-c-in-range-11-16 n t))
         (Cons e
               (subst-c-in-range-11-16 n t)))]))

(: combine (∀ (a)
              (-> (listt a) (listt a)
                  (listt a))))
(define (combine l1 l2)
  (match* (l1 l2)
    [((Empty) (Empty))
     (Empty)]
    [((Empty) (Cons a b))
     (Cons a b)]
    [((Cons a b) (Empty))
     (Cons a b)]
    [((Cons a b) (Cons e f))
     (Cons a
           (combine b (Cons e f)))]))

(: combine2 (∀ (a)
               (-> (listt a) (listt a)
                   (listt a))))
(define (combine2 l1 l2)
  (match l1
    [(Empty) l2]
    [(Cons a l)
     (Cons a (combine2 l l2))]))

(: combine-c (∀ (a)
                (-> (listt a)
                    (-> (listt a)
                        (listt a)))))
(define (combine-c l1)
  (lambda (l2)
    (match l1
      [(Empty) l2]
      [(Cons a l)
       (Cons a ((combine-c l) l2))])))

(: prefixer-123 (-> (listt Int)
                    (listt Int)))
(define (prefixer-123 l2)
  (Cons 1
        (Cons 2
              (Cons 3
                    l2))))

(: waiting-prefix-123 (-> (listt Int)
                          (listt Int)))
(define (waiting-prefix-123 l2)
  (Cons 1
        ((combine-c
          (Cons 2
                (Cons 3
                      (Empty))))
         l2)))

(: base (∀ (a)
           (-> a a)))
(define (base l2) l2)

(: combine-s (∀ (a)
                (-> (listt a)
                    (-> (listt a)
                        (listt a)))))
(define (combine-s ltt)
  (match ltt
    [(Empty) base]
    [(Cons a l1)
     (letrec ([make-cons
               : (∀ (a)
                    (-> a (-> (listt a) (listt a))
                        (-> (listt a)
                            (listt a))))
               (λ (a f)
                 (λ (l2)
                   (Cons a (f l2))))])
       (make-cons a (combine-s l1)))]))

(: prefix-3
   (-> (listt Int)
       (listt Int)))
(define (prefix-3 l2)
  (Cons 3 (base l2)))

(: prefix-23
   (-> (listt Int)
       (listt Int)))
(define (prefix-23 l2)
  (Cons 2 (prefix-3 l2)))

(: prefix-123
   (-> (listt Int)
       (listt Int)))
(define (prefix-123 l2)
  (Cons 1 (prefix-23 l2)))
