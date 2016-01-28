#lang racket

(define fatal
  (lambda (who . args)
    (display who) (display ": ")
    (for-each display args)
    (display "\n")
    (error 'infer "")))

;; unification
(define var (λ (v) (vector v)))
(define var? vector?)
(define empty-s '())

(define walk
  (λ (v s)
    (cond
      [(var? v)
       (cond
         [(assq v s) =>
          (λ (a)
            (walk (cdr a) s))]
         [else v])]
      [else v])))

(define walk*
  (λ (v s)
    (let ([v (walk v s)])
      (cond
        [(var? v) v]
        [(pair? v)
         (cons
          (walk* (car v) s)
          (walk* (cdr v) s))]
        [else v]))))

(define ext-s-check
  (lambda (x v s)
    (cond
      [(occurs-check x v s) #f]
      [else `((,x . ,v) . ,s)])))

(define occurs-check
  (lambda (x v s)
    (let ([v (walk v s)])
      (cond
        [(var? v) (eq? v x)]
        [(pair? v)
         (or
          (occurs-check x (car v) s)
          (occurs-check x (cdr v) s))]
        [else #f]))))

(define unify-check
  (lambda (v w s)
    (let ([v (walk v s)]
          [w (walk w s)])
      (cond
        [(eq? v w) s]
        [(var? v) (ext-s-check v w s)]
        [(var? w) (ext-s-check w v s)]
        [(and (pair? v) (pair? w))
         (cond
           [(unify-check (car v) (car w) s) =>
            (λ (s)
              (unify-check (cdr v) (cdr w) s))]
           [else #f])]
        [(equal? v w) s]
        [else #f]))))

(define reify-s
  (lambda (v s)
    (let ([v (walk v s)])
      (cond
        [(var? v)
         (ext-s-check v (reify-name (length s)) s)]
        [(pair? v) (reify-s (cdr v)
                            (reify-s (car v) s))]
        [else s]))))

(define reify
  (lambda (x s)
    (define reify-name
      (lambda (n)
        (string->symbol
         (string-append "t" (number->string n)))))
    (define reify1
      (lambda (x n s)
        (let ((x (walk x s)))
          (cond
            [(pair? x)
             (let*-values ([(u n1 s1) (reify1 (car x) n s)]
                           [(v n2 s2) (reify1 (cdr x) n1 s1)])
               (values (cons u v) n2 s2))]
            [(var? x)
             (let ([v* (reify-name n)])
               (values v* (add1 n) (ext-s-check x v* s)))]
            [else (values x n s)]))))
    (let*-values ([(x* n* s*) (reify1 x 0 s)]) x*)))

;; environment
(define ext-env (lambda (x v s) `((,x . ,v) . ,s)))

(define lookup
  (λ (x env)
    (cond
      [(assq x env) => cdr]
      [else (error 'lookup "unbound variable ~a" x)])))

(define env0
  `((zero? . (int -> bool))
    (add1  . (int -> int))
    (sub1  . (int -> int))
    (=     . (int -> (int -> bool)))
    (<=    . (int -> (int -> bool)))
    (<     . (int -> (int -> bool)))
    (>=    . (int -> (int -> bool)))
    (>     . (int -> (int -> bool)))
    (*     . (int -> (int -> int)))
    (-     . (int -> (int -> int)))
    (+     . (int -> (int -> int)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;; inferencer ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define infer1
  (lambda (exp env s)
    (match exp
      [(? number? x) (values 'int s)]
      [(? boolean? x) (values 'bool s)]
      [(? string? x) (values 'string s)]
      [(? symbol? x) (values (lookup x env) s)]
      [`(if ,test ,conseq ,alt)
       (let*-values ([(t1 s1) (infer1 test env s)]
                     [(s1^) (unify-check t1 'bool s1)])
         (cond
           [s1^
            (let*-values ([(t2 s2) (infer1 conseq env s1^)]
                          [(t3 s3) (infer1 alt env s2)]
                          [(s4) (unify-check t2 t3 s3)])
              (cond
                [s4 (values t3 s4)]
                [else
                 (fatal 'infer
                        "branches must have the same type       \n\n"
                        " - expression:        "  exp            "\n"
                        " - true branch type:  " (reify t2 s3)   "\n"
                        " - false branch type: " (reify t3 s3))  ]))]
           [else
            (fatal 'infer
                   "test is not of type bool       \n\n"
                   "expression:   " exp             "\n"
                   "irritant:     " test            "\n"
                   "type:         " (reify t1 s1)  )]))]
      [`(lambda (,x) ,body)
       (let*-values ([(t1) (var x)]
                     [(env*) (ext-env x t1 env)]
                     [(t2 s^) (infer1 body env* s)])
         (values `(,t1 -> ,t2) s^))]
      [`(,e1 ,e2)
       (let*-values ([(t1 s1) (infer1 e1 env s)]
                     [(t2 s2) (infer1 e2 env s1)]
                     [(t3) (var 't3)]
                     [(t4) (var 't4)]
                     [(s3) (unify-check t1 `(,t3 -> ,t4) s2)])
         (cond
           [(not s3)
            (fatal 'infer
                   "trying to apply non-function:\n\n"
                   " - irritant: " e1             "\n"
                   " - type:     " (reify t1 s1)    )]
           [else
            (let ([s4 (unify-check t2 t3 s3)])
              (cond
                [(not s4)
                 (fatal 'infer
                        "wrong argument type:              \n\n"
                        " - function:      " e1             "\n"
                        " - function type: " (reify t1 s3)  "\n"
                        " - expected type: " (reify t3 s3)  "\n"
                        " - argument type: " (reify t2 s3)  "\n"
                        " - argument:      " e2               )]
                [else
                 (values t4 s4)]))]))])))

(define infer
  (lambda (exp)
    (let*-values ([(t s) (infer1 exp env0 empty-s)])
      (reify t s))))
