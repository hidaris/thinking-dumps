#lang racket/base

;; (λx (λ y. y x)) Ω
(define Ω (λ () ((λ (x) (x x)) (λ (x) (x x)))))
((λ (x)
   (λ (y)
     (y x))) 'a)

;; (λx.(λy.(λz.z z) y) x)(λx.x x) =n (λa.a ((λg.g) a)) (λb.b b)
;; ->η (λy.(λz.z z) y) (λx.x x)      ->β (λb.b b) ((λg.g) (λb.b b))
;; ->η (λz.z z) (λx.x x)             ->β (λb.b b) (λb.b b)
;; ->α (λb.b b) (λb.b b)     = (λb.b b) (λb.b b) = Ω

;; λy.(λx.λy.x) (y y) =n λa.λb.(a a)
;; ->α λz.(λx.λy.x) (z z)
;; ->β λz.λy.(z z) ->α λa.λb.(a a)

;; (λf.λg.λx.f x (g x))(λx.λy.x)(λx.λy.x) =n λx.x
;; ->β λx.(λx.λy.x) x ((λx.λy.x) x)
;; ->β λx.x

(define True
  (λ (x)
    (λ (y) x)))

(define False
  (λ (x)
    (λ (y) y)))

(define If
  (λ (v)
    (λ (t)
      (λ (f)
        ((v t) f)))))

;; (λv.λt.λf.v t f) (λx.λy.x) =n (λx.λy.y)
;; ->β λt.λf.(λx.y.x) t f ->β λt.λf.t
;;             |   |  |
;; ->α λx.λy.y

;; if true =n true
;; if false =n false

;; and true false =n false
;; and =n λx.λy.x y false
;; or true false =n true
;; or =n λx.λy.x true y

;; not =n λx.x false true

;; mkpair M N= λs.if s M N
;; =n λs.s M N
;; mkpair =n λm.λn.λs s m n
;; fst =n λp.p true
;; snd =n λp.p false

(define ZERO  (λ (f) (λ (x) x)))
(define ONE   (λ (f) (λ (x) (f x))))
(define TWO   (λ (f) (λ (x) (f (f x)))))
(define THREE (λ (f) (λ (x) (f (f (f x))))))

(define (encode n) ((n add1) 0))

(define ADD1 (λ (n) (λ (f) (λ (x) (f ((n f) x))))))

(define c2n
  (λ (n)
    (cond
      [(zero? n) ZERO]
      [else (ADD1 (c2n (sub1 n)))])))

(define (encode-add1 n)
  (((ADD1 n) add1) 0))

(define ADD (λ (n) (λ (m) ((m ADD1) n))))

(define (encode-add n m)
  (encode ((ADD n) m)))

(define iszero (λ (n) ((n (λ (x) False)) True)))

(define mkpair (λ (m) (λ (n) (λ (s) ((s m) n)))))
(define fst (λ (p) (p True)))
(define snd (λ (p) (p False)))


(define wrap (λ (f) (λ (p) ((mkpair False) (((If (fst p)) (snd p)) (f (snd p)))))))
(define Sub1 (λ (n) (λ (f) (λ (x) (snd ((n (wrap f)) ((mkpair True) x)))))))

;; (define Mult (λ (n) (λ (m) (((If (iszero m)) (((Sub1 n) (ADD m)) m)) (((Sub1 m) (ADD n)) n)))))

;; (define mkMult0
;;   (λ (t)
;;     (λ (n)
;;       (λ (m)
;;         (((If (iszero n)) ZERO) ((ADD m) ((t (Sub1 n)) m)))))))

(define (IfC v t f)
  (((If v) t) f))

(define (ADDC n m)
  ((ADD n) m))

;; (define mkMult1
;;   (λ (t)
;;     (λ (n)
;;       (λ (m)
;;         (IfC (iszero n) ZERO (ADDC m (((t t) (Sub1 n)) m)))))))

;; (define mult (mkMult1 mkMult1))
;; can't work

(define mkMult2
  (λ (t)
    (λ (n)
      (λ (m)
        (if (zero? n) 0 (+ m (((t t) (sub1 n)) m)))))))

(define Mult (mkMult2 mkMult2))
