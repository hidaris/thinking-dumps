#lang racket

;; <program-terms> ::= identifiers
;;                     applications
;;                     type applications
;;                     abstraction
;;                     type abstraction
;;                     primitive numbers
;;                     successor function
(struct ID (sym))
(struct APP (rator rand))
(struct TYAPP (rator rand))
(struct ABS (typ sym body))
(struct TYABS (sym body))
(struct NUM (val) #:transparent)
(struct SUCC ())

;; <type-terms> ::= type identifiers
;;                  arrows
;;                  type arrows
;;                  primitive number type
(struct TYID (sym) #:transparent)
(struct ARR (dom rng) #:transparent)
(struct TYARR (sym body) #:transparent)
(struct TYNUM () #:transparent)

(define eval
  (match-lambda
    [(APP rator rand)
     (match (eval rator)
       [(ABS _ id body)
        (eval (subst id eval rand) body)]
       [(SUCC)
        (match (eval rand)
          [(NUM n)
           (NUM (add1 n))])])]
    [(TYAPP rator rand)
     (match (eval rator)
       [(TYABS id body)
        (eval (type-subst id rand body))]
       [val
        val])]))

(define (subst x v t)
  (match t
    [(ID sym)
     (if (eq? x sym)
         v
         t)]
    [(SUCC)
     t]
    [(APP rator rand)
     (APP (subst x v rator)
          (subst x v rand))]
    [(ABS ty id body)
     (if (eq? id x)
         t
         (ABS ty id (subst x v body)))]))

(define (type-subst x v t)
  (match t
    [(SUCC)
     t]
    [(TYID sym)
     (if (eq? sym x)
         v
         t)]
    [(APP rator rand)
     (APP (type-subst x v rator)
          (type-subst x v rand))]
    [(ABS ty id body)
     (ABS (type-subst x v ty)
          id
          (type-subst x v body))]
    [(ARR dom rng)
     (ARR (type-subst x v dom)
          (type-subst x v rng))]
    [(ID sym)
     t]))

(define (type-of t)
  (type-of/env (hasheq) t))
(define (type-of/env env t)
  (match t
    [(ID sym)
     (hash-ref env sym)]
    [(ABS ty id body)
     (ARR ty (type-of/env (hash-set env id ty) body))]
    [(APP rator rand)
     (match (type-of/env env rator)
       [(ARR dom rng)
        (and (equal? dom (type-of/env env rand))
             rng)]
       [_
        #f])]
    [(TYAPP rator rand)
     (match (type-of/env env rator)
       [(TYARR id body)
        (type-of/env env (type-subst id rand body))]
       [_
        #f])]
    [(TYABS id body)
     (TYARR id body)]
    [(SUCC)
     (ARR (TYNUM) (TYNUM))]
    [(NUM _)
     (TYNUM)]
    [_
     #f]))

(define DOUBLE
  (TYABS 'X
         (ABS (ARR (TYID 'X) (TYID 'X))
              'f
              (ABS (TYID 'X)
                   'a
                   (APP (ID 'f)
                        (APP (ID 'f)
                             (ID 'a)))))))


(APP
 (APP (TYAPP DOUBLE (TYNUM))
      (SUCC))
 (NUM 3))
