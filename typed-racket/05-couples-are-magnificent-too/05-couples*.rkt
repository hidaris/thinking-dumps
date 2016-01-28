#lang typed/racket

(define-type (pizza a)
  (U Bottom (Topping a)))

(struct Bottom ()
  #:transparent)
(struct (a) Topping ([v : a] [w : (pizza a)])
  #:transparent)

(define-type fish
  (U Anchovy
     Lox
     Tuna))

(struct Anchovy () #:transparent)
(struct Lox () #:transparent)
(struct Tuna () #:transparent)

(define-type num
  (U Zero One-more-than))

(struct Zero ()
  #:transparent)
(struct One-more-than ([v : num])
  #:transparent)

(: rem-anchovy (-> (pizza fish)
                   (pizza fish)))
(define (rem-anchovy pizza)
  (match pizza
    [(Bottom) (Bottom)]
    [(Topping (Anchovy) p)
     (rem-anchovy p)]
    [(Topping t p)
     (Topping t (rem-anchovy p))]))

(: rem-tuna (-> (pizza fish)
                (pizza fish)))
(define (rem-tuna pizza)
  (match pizza
    [(Bottom) (Bottom)]
    [(Topping (Tuna) p)
     (rem-tuna p)]
    [(Topping t p)
     (Topping t (rem-tuna p))]))

(: rem-fish (-> fish (pizza fish)
                (pizza fish)))
(define (rem-fish f pizza)
  (match pizza
    [(Bottom) (Bottom)]
    [(Topping t p)
     (match f
       [t (rem-fish f p)]
       [_ (Topping t (rem-fish f p))])]))

(: eq-fish (-> fish fish Boolean))
(define (eq-fish f1 f2)
  (match* (f1 f2)
    [((Anchovy) (Anchovy))
     true]
    [((Lox) (Lox))
     true]
    [((Tuna) (Tuna))
     true]
    [(_ _) false]))

(: rem-fish2 (-> fish (pizza fish)
                 (pizza fish)))
(define (rem-fish2 x pizza)
  (match pizza
    [(Bottom) (Bottom)]
    [(Topping t p)
     (if (eq-fish t x)
         (rem-fish2 x p)
         (Topping t
                  (rem-fish2 x p)))]))

(: eq-int (-> Integer Integer
              Boolean))
(define (eq-int n m) (= n m))

(: rem-int (-> Integer (pizza Integer)
               (pizza Integer)))
(define (rem-int x pizza)
  (match pizza
    [(Bottom) (Bottom)]
    [(Topping t p)
     (if (eq-int t x)
         (rem-int x p)
         (Topping t (rem-int x p)))]))

(: subst-fish (-> fish fish (pizza fish)
                  (pizza fish)))
(define (subst-fish n a pizza)
  (match pizza
    [(Bottom) (Bottom)]
    [(Topping t p)
     (if (eq-fish t a)
         (Topping n
                  (subst-fish n a p))
         (Topping t
                  (subst-fish n a p)))]))

(: subst-int (-> Integer Integer (pizza Integer)
                 (pizza Integer)))
(define (subst-int n a pizza)
  (match pizza
    [(Bottom) (Bottom)]
    [(Topping t p)
     (if (eq-int t a)
         (Topping n
                  (subst-int n a p))
         (Topping t
                  (subst-int n a p)))]))

(: eq-num (-> num num
              Boolean))
(define (eq-num n1 n2)
  (match* (n1 n2)
    [((Zero) (Zero))
     true]
    [((One-more-than n) (One-more-than n))
     true]
    [(_ _) false]))
