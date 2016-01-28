#lang typed/racket

(define-type pizza
  (U Crust
     Cheese
     Onion
     Anchovy
     Sausage))

(struct Crust () #:transparent)
(struct Cheese ([v : pizza]) #:transparent)
(struct Onion ([v : pizza]) #:transparent)
(struct Anchovy ([v : pizza]) #:transparent)
(struct Sausage ([v : pizza]) #:transparent)

(: remove_anchovy (-> pizza pizza))
(define (remove_anchovy pz)
  (match pz
    [(Crust) (Crust)]
    [(Cheese x) (Cheese (remove_anchovy x))]
    [(Onion x) (Onion (remove_anchovy x))]
    [(Anchovy x) (remove_anchovy x)]
    [(Sausage x) (Sausage (remove_anchovy x))]))

(: top_anchovy_with_cheese (-> pizza pizza))
(define (top_anchovy_with_cheese pz)
  (match pz
    [(Crust) (Crust)]
    [(Cheese x)
     (Cheese (top_anchovy_with_cheese x))]
    [(Onion x)
     (Onion (top_anchovy_with_cheese x))]
    [(Anchovy x)
     (Cheese (Anchovy (top_anchovy_with_cheese x)))]
    [(Sausage x)
     (Sausage (top_anchovy_with_cheese x))]))

(: subst_anchovy_by_cheese (-> pizza pizza))
(define (subst_anchovy_by_cheese pz)
  (match pz
    [(Crust) (Crust)]
    [(Cheese x)
     (Cheese (subst_anchovy_by_cheese x))]
    [(Onion x)
     (Onion (subst_anchovy_by_cheese x))]
    [(Anchovy x)
     (Cheese (subst_anchovy_by_cheese x))]
    [(Sausage x)
     (Sausage (subst_anchovy_by_cheese x))]))
