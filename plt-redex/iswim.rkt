#lang racket
(require redex)

(define-language iswim
  ; grammar
  ((M N L K) X (λ X M) (M M) b (o2 M M) (o1 M))
  ; primitive operations
  (o o1 o2)
  (b number)
  (o1 add1 sub1 iszero)
  (o2 + - * ^)
  ; values =>
  ((V U W) b X (λ X M))
  (E hole (V E) (E M) (o V ... E M ...))
  ((X Y Z) variable-not-otherwise-mentioned))

; δ = b1 U b2
(define-metafunction iswim
  ; b1
  [(δ (iszero 0)) (λ x (λ y x))]
  [(δ (iszero b)) (λ x (λ y y))]
  [(δ (add1 b)) ,(add1 (term b))]
  [(δ (sub1 b)) ,(sub1 (term b))]
  ; b2
  [(δ (+ b_1 b_2)) ,(+ (term b_1) (term b_2))]
  [(δ (- b_1 b_2)) ,(- (term b_1) (term b_2))]
  [(δ (* b_1 b_2)) ,(* (term b_1) (term b_2))]
  [(δ (^ b_1 b_2)) ,(expt (term b_1) (term b_2))])

(define-metafunction iswim
  ;; 1. X_1 bound, so don't continue in λ body
  [(subst (λ X_1 any_1) X_1 any_2)
   (λ X_1 any_1)]

  ;; 2. do capture avoiding substitution
  ;;    by generating a fresh name
  [(subst (λ X_1 any_1) X_2 any_2)
   (λ X_3
     (subst (subst-var any_1 X_1 X_3) X_2 any_2))
   (where X_3 ,(variable-not-in (term (X_2 any_1 any_2))
                                (term X_1)))]

  ;; 3. replace X_1 with any_1
  [(subst X_1 X_1 any_1) any_1]

  ;; the last two cases just recur on
  ;; the tree structure of the term
  [(subst (any_2 ...) X_1 any_1)
   ((subst any_2 X_1 any_1) ...)]
  [(subst any_2 X_1 any_1) any_2])
