#lang typed/racket

(define-type tvar Integer)

(define freshsource 0)

(define (fresh)
  (let* ([tmp freshsource]
         [_ (set! freshsource
                  (+ freshsource 1))])
    tmp))

(define-type monotype
  (U TBool
     TArr
     TVar))

(struct TBool
  ()
  #:transparent)
(struct TArr
  ([a : monotype]
   [b : monotype])
  #:transparent)
(struct TVar
  ([v : tvar])
  #:transparent)

(define-type polytype
  PolyType)

(struct PolyType
  ([a : (Listof Integer)]
   [b : monotype])
  #:transparent)

(define-type exp
  (U True
     False
     Var
     App
     Let
     Fn
     If))

(struct True
  ()
  #:transparent)
(struct False
  ()
  #:transparent)
(struct Var
  ([v : Integer])
  #:transparent)
(struct App
  ([a : exp] [b : exp])
  #:transparent)
(struct Let
  ([a : exp] [b : exp])
  #:transparent)
(struct Fn
  ([a : exp])
  #:transparent)
(struct If
  ([a : exp] [b : exp] [c : exp])
  #:transparent)

(define-type info
  (U PolyTypeVar
     MonoTypeVar))

(struct
  PolyTypeVar ([v : polytype])
  #:transparent)

(struct
  MonoTypeVar ([v : monotype])
  #:transparent)

(define-type-alias context
  (Listof info))

(: subst : (monotype tvar monotype -> monotype))
(define (subst ty2 var ty)
  (match ty
    [(TVar var2)
     (if (eq? var var2)
         ty2
         (TVar var2))]
    [(TArr l r)
     (TArr (subst ty2 var l)
           (subst ty2 var r))]
    [(TBool)
     (TBool)]))

(: freevars : (monotype -> (Listof tvar)))
(define (freevars t)
  (match t
    [(TVar v) `(,v)]
    [(TArr l r)
     (append (freevars l)
             (freevars r))]
    [(TBool)
     '()]))

(: dedup : ((Listof tvar) -> (Listof tvar)))
(define (dedup lst)
  (match lst
    [`() `()]
    [`(,x . ,xs)
     (if (exists (λ (y) (eqv? x y)) xs)
         (dedup xs)
         (cons x (dedup xs)))]))

(: exists : ((tvar -> Boolean) (Listof tvar) -> Boolean))
(define (exists f xs)
  (cond [(null? xs) false]
        [(f (car xs)) true]
        [else (exists f (cdr xs))]))

;; (define (generalize-monotype ctx ty)
;;   (letrec ([notMem (lambda (xs)
;;                      (lambda (x)
;;                        (all (λ (y) (not (eqv? x y))) xs)))]
;;            [all (lambda (f ls)
;;                   (cond
;;                     [(null? ls) '()]
;;                     [(f (car ls))
;;                      (cons (car ls) (all f (cdr ls)))]
;;                     [else (all f (cdr ls))]))]
;;            [free (lambda (t)
;;                    (match t
;;                      [(MonoTypeVar m)
;;                       (freevars m)]
;;                      [(PolyTypeVar (PolyType bs m))
;;                       (filter (notMem bs) (freevars m))]))])
;;     (let* ([ctxvars (flatten (map free ctx))]
;;            [polyvars (filter (notMem ctxvars) (freevars ty))])
;;       (PolyType (dedup polyvars ty)))))

;; (define (min-new-monotype))
