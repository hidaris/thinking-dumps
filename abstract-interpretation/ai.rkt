#lang racket

;; environment
(define empty-env
  (λ ()
    (λ (y)
      (error 'interp "unbound identifier. ~s" y))))

(define extend-env
  (λ (var val env)
    `((,var . ,val) . ,env)))

(define apply-env
  (λ (env var)
    (cond
      [(assq var env) => cdr]
      [else (error 'env "unbound variable. ~s" var)])))

(define closure
  (λ (x body env)
    `(closure ,x ,body ,env)))

(define apply-closure
  (λ (closure arg)
    (match closure
      [`(closure ,x ,body ,env)
       (value-of body (extend-env x arg env))])))

;;; helper functions
(define (union set1 set2)
  (cond
    [(eq? set1 null) set2]
    [(eq? set2 null) set1]
    [else
     (if (not (memv (car set1) set2))
         (cons (car set1)
               (union (cdr set1)
                      (remv (car set1) set2)))
         (union (cdr set1) set2))]))

(define set-diff
  (lambda (set1 set2)
    (cond
      [(null? set1) '()]
      [(null? set2) set1]
      [else
       (set-diff
        (filter (lambda (x)
                  (not (eq? x (car set2))))
                set1)
        (cdr set2))])))

(define (unique-free-vars exp)
  (match exp
    [(? symbol? x) `(,x)]
    [`(lambda (,x) ,body)
     (remv x (unique-free-vars body))]
    [`(,rator,rand)
     (union (unique-free-vars rator) (unique-free-vars rand))]))

(define free-vars unique-free-vars)

(define value-of
  (λ (exp env)
    (match exp
      [(? number? n) n]
      [(? boolean? b) b]
      [(? symbol? x) (apply-env env x)]
      [`(zero? ,n) (zero? (value-of n env))]
      [`(+ ,e1 ,e2) (+ (value-of e1 env)
                       (value-of e2 env))]
      [`(- ,e1 ,e2) (- (value-of e1 env)
                       (value-of e2 env))]
      [`(* ,e1 ,e2) (* (value-of e1 env)
                       (value-of e2 env))]
      [`(if ,test ,conseq ,alt) (if (value-of test env)
                                    (value-of conseq env)
                                    (value-of alt env))]
      [`(lambda (,formals) ,body) (closure formals body env)]
      [`(,rator ,rand) (apply-closure (value-of rator env)
                                      (value-of rand env))])))

(define Ds-zero?
  (λ (x)
    (match x
      [`bottom 'bottom]
      [(? integer? x) (zero? x)]
      [else `top])))

(define Ds-plus
  (λ (x y)
    (match `(,x ,y)
      [`(bottom ,y) 'bottom]
      [`(,x bottom) 'bottom]
      [`(,(? integer? x) ,(? integer? y))
       (cond
         [(or (zero? x) (zero? y)) (+ x y)]
         [(= x y) x]
         [else 'top])]
      [else 'top])))

(define Ds-minus
  (λ (x y)
    (match `(,x ,y)
      [`(bottom ,y) 'bottom]
      [`(,y bottom) 'bottom]
      [`(,any ,(? integer? y)) (Ds-plus any (- y))]
      [else `top])))

(define Ds-times
  (λ (x y)
    (match `(,x ,y)
      [`(bottom ,y) 'bottom]
      [`(,x bottom) 'bottom]
      [`(,(? integer? x) ,(? integer? y)) (* x y)]
      [else 'top])))

(define Ds-int
  (λ (x)
    (cond
      [(positive? x) 1]
      [(negative? x) -1]
      [else 0])))

(define Ds-bool
  (λ (x)
    'bottom))

(define id identity)

(define Di-int id)

(define Di-bool id)

(define Di-zero? zero?)

(define Di-plus +)

(define Di-minus -)

(define Di-times *)

(define Di-if
  (λ (eval)
    (λ (test-exp then-exp else-exp env)
      (if (eval test-exp env)
          (eval then-exp env)
          (eval else-exp env)))))

(define potentially-apply-procedure
  (λ (eval)
    (λ (proc args)
      (match proc
        [`(closure ,formals ,body ,env)
         (eval body (extend-env formals args env))]
        [else (error 'potentially-apply-procedure
                     "Cannot apply closure ~s" proc)]))))

;; (define Di-apply-hov
;;   (λ (eval)
;;     (let ([apply-procedure (potentially-apply-procedure eval)])
;;       (λ (hov args)
;;         (apply-procedure hov args)))))
;; hov means higher-order-value
(define Di-apply-hov
  potentially-apply-procedure)

;; Di-closure could be the identity, but we shall introduce a new idea
;; Here is the definition, which relies on the meaning of the higher-order
;; -value cache **hov-cache**.
(define **hov-cache** '())

(define cache-maker
  (λ (binary-predicate? not-found-value-constructor)
    (let ([cache '()])
      (λ (target)
        (letrec
          ([lookup
            (λ (table)
              (cond
                [(null? table)
                 (let ([value (not-found-value-constructor target)])
                   (set! cache (cons `(,target . ,value) cache))
                   value)]
                [(binary-predicate? target (caar table)) (cdar table)]
                [else (lookup (cdr table))]))])
          (lookup cache))))))

(define initialize-hov-cache
  (λ ()
    (set! **hov-cache** (cache-maker Di-closure-equiv? (λ (target) target)))))

(define Di-closure
  (λ (closure)
    (**hov-cache** closure)))

(define Di-closure-equiv?
  (λ (cl-x cl-y)
    (match `(,cl-x ,cl-y)
      [`((closure ,formals-x ,body-x ,env-x)
         (closure ,formals-y ,body-y ,env-y))
       (and
        (eq? body-x body-y)
        (andmap
         (λ (var)
           (Di-equiv? (apply-env env-x var)
                      (apply-env env-y var)))
         (set-diff (free-vars body-x) formals-x)))])))

(define Di-equiv?
  (λ (x y)
    (match `(,x ,y)
      [`(,(? integer? x) ,(? integer? y)) (= x y)]
      [`((closure ,formals-x ,body-x ,env-x)
         (closure ,formals-y ,body-y ,env-y))
       (Di-closure-equiv? x y)]
      [else #f])))

; integer domain interpreter
(define potentially-recursive-eval-maker
  (λ (D-int D-bool D-zero? D-plus D-minus D-times D-if D-closure D-apply-hov)
    (λ (eval)
      (λ (exp env)
        (match exp
          [(? integer? int) (D-int int)]
          [(? boolean? bool) (D-bool bool)]
          [(? symbol? var) (apply-env env var)]
          [`(zero? ,e1) (D-zero? (eval e1 env))]
          [`(+ ,e1 ,e2) (D-plus (eval e1 env) (eval e2 env))]
          [`(- ,e1 ,e2) (D-minus (eval e1 env) (eval e2 env))]
          [`(* ,e1 ,e2) (D-times (eval e1 env) (eval e2 env))]
          [`(if ,test-exp ,then-exp ,else-exp)
           ((D-if eval) test-exp then-exp else-exp env)]
          [`(lambda ,formals ,body) (D-closure `(closure ,formals ,body ,env))]
          [`(,rator ,rands ...)
           ((D-apply-hov eval) (eval rator env)
            (map (λ (x) (eval x env)) `(,rands ...)))])))))

(define potentially-recursive-eval
  (potentially-recursive-eval-maker
   Di-int Di-bool Di-zero? Di-plus Di-minus Di-times Di-if Di-closure Di-apply-hov))

(define potentially-recursive-factorial
  (λ (factorial)
    (λ (n)
      (if (zero? n)
          1
          (* n (factorial (- n 1)))))))

;; (define f
;;   (λ (!)
;;     (λ (n)
;;       (if (zero? n)
;;           1
;;           (* n (! (- n 1)))))))

;; (define fixo
;;   (λ (potentially-recursive-factorial)
;;     (let ([prf* (λ (prf**)
;;                   (potentially-recursive-factorial
;;                    (λ (x)
;;                      ((prf** prf**) x))))])
;;       (prf* prf*))))

;; (define fixo
;;   (λ (f)
;;     (let ([prf* (λ (prf**)
;;                   (f
;;                    (λ (x)
;;                      ((prf** prf**) x))))])
;;       (prf* prf*))))

;; (define fixo
;;   (λ (f)
;;     ((λ (prf**)
;;        (f
;;         (λ (x)
;;           ((prf** prf**) x))))
;;      (λ (prf**)
;;        (f
;;         (λ (x)
;;           ((prf** prf**) x)))))))

;; applicative-order Y
(define fix
  (λ (f)
    ((λ (!)
       (f
        (λ (x)
          ((! !) x))))
     (λ (!)
       (f
        (λ (x)
          ((! !) x)))))))

(let ([factorial (fix potentially-recursive-factorial)])
  (factorial 4))

(define fix-maker
  (λ (binary-op predicate?)
    (λ (memo thunk)
      (letrec
        ([fix-loop
          (λ ()
            (let ([value (binary-op (thunk) (mcdr memo))])
              (cond
                [(predicate? value (mcdr memo)) value]
                [else (begin
                        (set-mcdr! memo value)
                        (fix-loop))])))])
        (fix-loop)))))

;; ((fix-maker max (λ (m x) (= (- m 1) x)))
;;  (mcons 'a 0)
;;  (λ () (random 10)))

(define fix-maker2
  (λ (binary-op predicate?)
    (λ (memo thunk)
      (letrec
        ([fix-loop
          (λ (memo)
            (let ([value (binary-op (thunk) memo)])
              (cond
                [(predicate? value memo) value]
                [else (fix-loop value)])))])
        (fix-loop memo)))))

;; ((fix-maker2 max (λ (m x) (= (- m 1) x)))
;;  0
;;  (λ () (random 10)))

(define fix-memo
  (fix-maker max (λ (m x) (= (- m 1) x))))

(define fix-finite
  (λ (potentially-recursive-eval)
    (let ([stack '()])
      (letrec
        ([eval (potentially-recursive-eval
                (λ (exp env)
                  (cond
                    [(assq exp stack) => cdr]
                    [else (let ([memo (cons exp 'bottom)]
                                [thunk (λ () (eval exp env))])
                            (set! stack (cons memo stack))
                            (let ([value (fix-memo memo thunk)])
                              (set! stack (cdr stack))
                              value))])))])
        eval))))
