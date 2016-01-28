#lang racket
(require redex)

;; (define-language L
;;   (M ::= N F (M ...))
;;   (F ::= fred wilma)
;;   (N ::= 2 7))

;; (define-metafunction L
;;   swap : M -> M
;;   [(swap fred) wilma]
;;   [(swap wilma) fred]
;;   [(swap (M ...)) ((swap M) ...)]
;;   [(swap M) M])

(define-language PCF
  (M ::=
     N O X L
     (μ (X : T) L)
     (M M ...)
     (if0 M M M))
  (X ::= variable-not-otherwise-mentioned)
  (L ::= (λ ([X : T] ...) M))
  (V ::= N O L)
  (N ::= number)
  (O ::= O1 O2)
  (O1 ::= add1 sub1)
  (O2 ::= + *)
  (T ::= num (T ... -> T)))

(define-term fact-5
  ((μ (fact : (num -> num))
       (λ ([n : num])
         (if0 n
              1
              (* n (fact (sub1 n))))))
   5))

(define-extended-language PCFT PCF
  (Γ ::= ((X T) ...)))

(define-language REDEX)

(define-judgment-form REDEX
  #:mode (lookup I I O)
  #:contract (lookup ((any any) ...) any any)
  [(lookup (_ ... (any any_0) _ ...) any any_0)])

(define-metafunction REDEX
  ext1 : ((any any) ...) (any any) -> ((any any) ...)
  [(ext1 (any_0 ... (any_k any_v0) any_1 ...) (any_k any_v1))
   (any_0 ... (any_k any_v1) any_1 ...)]
  [(ext1 (any_0 ...) (any_k any_v1))
   ((any_k any_v1) any_0 ...)])

(define-metafunction REDEX
  ext : ((any any) ...) (any any) ... -> ((any any) ...)
  [(ext any) any]
  [(ext any any_0 any_1 ...)
   (ext1 (ext any any_1 ...) any_0)])

;; (define-metafunction REDEX
;;   unique : any ... -> boolean
;;   [(unique any_!_1 ...) #t]
;;   [(unique _ ...) #f])

(define-relation REDEX
  unique ⊆ any x ...
  [(unique any_!_1 ...)])

(define-judgment-form PCFT
  #:mode (⊢ I I I O)
  #:contract (⊢ Γ M : T)
  [(lookup Γ X T)
   -------------- var
   (⊢ Γ X : T)]
  [-------------- num
   (⊢ Γ N : num)]
  [----------------------- op1
   (⊢ Γ O1 : (num -> num))]
  [--------------------------- op2
   (⊢ Γ O2 : (num num -> num))]
  [(⊢ Γ M_1 : num)
   (⊢ Γ M_2 : T)
   (⊢ Γ M_3 : T)
   --------------------------- if0
   (⊢ Γ (if0 M_1 M_2 M_3) : T)]
  [(⊢ (ext Γ (X T)) L : T)
   ------------------------ μ
   (⊢ Γ (μ (X : T) L) : T)]
  [(⊢ Γ M_0 : (T_1 ..._1 -> T))
   (⊢ Γ M_1 : T_1) ...
   ------------------------ app
   (⊢ Γ (M_0 M_1 ..._1) : T)]
  [(unique X ...)
   (⊢ (ext Γ (X T) ...) M : T_n)
   ------------------------------------------ λ
   (⊢ Γ (λ ([X : T] ...) M) : (T ... -> T_n))])

(define r
  (reduction-relation
   PCF #:domain M
   (--> (μ (X : T) M)
        (subst (X (μ (X : T) M)) M)
        μ)

   (--> ((λ ([X : T] ...) M_0) M ...)
        (subst (X M) ... M_0)
        β)

   (--> (O N_0 ...) N_1
        (judgment-holds (δ (O N_0 ...) N_1))
        δ)

   (--> (if0 0 M_1 M_2) M_1 if-t)
   (--> (if0 N M_1 M_2) M_2
        (side-condition (not (zero? (term N))))
        if-f)))

(define-judgment-form PCF
  #:mode (δ I O)
  #:contract (δ (O N ...) N)
  [(δ (+ N_0 N_1) ,(+ (term N_0) (term N_1)))]
  [(δ (* N_0 N_1) ,(* (term N_0) (term N_1)))]
  [(δ (sub1 N) ,(sub1 (term N)))]
  [(δ (add1 N) ,(add1 (term N)))])

(define -->r (compatible-closure r PCF M))

(define-extended-language PCFn PCF
  (E ::= hole
     (E M ...)
     (O V ... E M ...)
     (if0 E M M)))

(define -->n
  (context-closure r PCFn E))

(define-extended-language PCFv PCF
  (E ::= hole
     (V ... E M ...)
     (if0 E M M)))

(define v
  (extend-reduction-relation
   r PCF #:domain M
   (--> ((λ ([X : T] ...) M_0) V ...)
        (subst (X V) ... M_0)
        β)))

(define -->v
  (context-closure v PCFv E))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Substitution

(define-language L
  (T any)
  (M any)
  (X (variable-except λ μ if0 : num)))

(define-metafunction L
  subst : (X M) ... M -> M
  [(subst (X_1 M_1) (X_2 M_2) ... M_3)
   (subst-1 X_1 M_1 (subst (X_2 M_2) ... M_3))]
  [(subst M_3) M_3])

(define-metafunction L
  subst-1 : X M M -> M
  ;; 1. X_1 bound, so don't continue in λ body
  [(subst-1 X_1 M_1 (λ ([X_2 : T_2] ... [X_1 : T_1] [X_3 : T_3] ...) M_2))
   (λ ([X_2 : T_2] ... [X_1 : T_1] [X_3 : T_3] ...) M_2)
   (side-condition (not (member (term X_1) (term (X_2 ...)))))]
  ;; or μ
  [(subst-1 X M_1 (μ (X : T) M_2))
   (μ (X : T) M_2)]

  ;; 2. general purpose capture avoiding case
  [(subst-1 X_1 M_1 (λ ([X_2 : T_2] ...) M_2))
   (λ ([X_new : T_2] ...) (subst-1 X_1 M_1 (subst-vars (X_2 X_new) ... M_2)))
   (where (X_new ...) ,(variables-not-in (term (X_1 M_1 M_2)) (term (X_2 ...))))]
  ;; and μ
  [(subst-1 X_1 M_1 (μ (X_2 : T) M_2))
   (μ (X_new : T) (subst-1 X_1 M_1 (subst-vars (X_2 X_new) M_2)))
   (where (X_new) ,(variables-not-in (term (X_1 M_1 M_2)) (term (X_2))))]

  ;; 3. replace X_1 with M_1
  [(subst-1 X_1 M_1 X_1) M_1]
  ;; 4. X_1 and X_2 are different, so don't replace
  [(subst-1 X_1 M_1 X_2) X_2]
  ;; the last cases cover all other expressions
  [(subst-1 X_1 M_1 (M_2 ...)) ((subst-1 X_1 M_1 M_2) ...)]
  [(subst-1 X_1 M_1 M_2) M_2])

(define-metafunction L
  subst-vars : (X M) ... M -> M
  [(subst-vars (X_1 M_1) X_1) M_1]
  [(subst-vars (X_1 M_1) (M_2 ...))
   ((subst-vars (X_1 M_1) M_2) ...)]
  [(subst-vars (X_1 M_1) M_2) M_2]
  [(subst-vars (X_1 M_1) (X_2 M_2) ... M_3)
   (subst-vars (X_1 M_1) (subst-vars (X_2 M_2) ... M_3))]
  [(subst-vars M) M])
