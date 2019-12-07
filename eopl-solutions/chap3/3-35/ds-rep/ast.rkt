#lang typed/racket

(provide (all-defined-out))

;;; Env
(define-type Environment
  (U EmptyEnv ExtendEnv ExtendEnvRec))

(struct empty-env
  ()
  #:transparent)

(define-type EmptyEnv empty-env)

(struct extend-env
  ([var : Symbol]
   [val : (U Value (Mutable-Vectorof Any))]
   [env : Environment])
  #:transparent)

(define-type ExtendEnv extend-env)

(struct extend-env-rec
  ([pname  : Symbol]
   [bvars  : (Listof Symbol)]
   [body   : Expression]
   [env    : Environment])
  #:transparent)

(define-type ExtendEnvRec extend-env-rec)

(: extend-env-rec*
   ((Listof Symbol)
    (Listof (Listof Symbol))
    (Listof Expression)
    Environment
    ->
    Environment))
(define (extend-env-rec* pnames bvar-lsts pbodys env)
  (append
   (map proc-item-maker pnames bvar-lsts pbodys) env))

(: proc-item-maker
   (-> Symbol (Listof Symbol) Expression
       (Listof (U Symbol (Listof Symbol) Expression))))
(define proc-item-maker
  (λ (pname bvars body)
    (list pname bvars body)))

(: assoc
   (-> Symbol
       (Listof (Listof (U Symbol (Listof Symbol) Expression)))
       (U #f (Listof (U Symbol (Listof Symbol) Expression)))))
(define (assoc var table)
  (cond
    [(null? table) #f]
    [(equal? (caar table) var) (car table)]
    [else (assoc var (cdr table))]))

(: rebuild-env
   (-> Environment Environment))
(define (rebuild-env env)
  (match env
    [(empty-env) (empty-env)]
    [(extend-env var val saved-env)
     #:when (not (vector? val))
     (extend-env var val (rebuild-env env))]
    [(extend-env-rec pname bvars body saved-env)
     (let ([vec (make-vector 1)])
       (let ([new-env (extend-env pname vec (rebuild-env saved-env))])
         (vector-set! vec 0 (Closure (RecVal-vars val)
                                     (RecVal-body val)
                                     new-env
                                     #f))
         new-env))
     (define proc-table
       (map proc-item-maker pnames bvars-lst bodys))
     ]))

(: apply-env
   (-> Symbol Environment Value))
(define (apply-env var env)
  (match env
    [(empty-env)
     (error 'apply-env
            "var ~s doesn't bound to a value" var)]
    [(extend-env saved-var saved-val saved-env)
     (if (eqv? var saved-var)
         saved-val
         (apply-env var saved-env))]
    [(extend-env-rec pnames bvars-lst bodys saved-env)
     (define proc-table
       (map proc-item-maker pnames bvars-lst bodys))
     (define has-proc (assoc var proc-table))
     (if has-proc
         (let ([b-var (cast (cadr has-proc) (Listof Symbol))]
               [p-body (cast (caddr has-proc) Expression)])
           (Closure b-var p-body env #f))
         (apply-env var saved-env))
     ]))

(: init-env
   (-> Environment))
(define (init-env)
  (extend-env
    'i (Num 1)
    (extend-env
      'v (Num 5)
      (extend-env
        'x (Num 10)
        (empty-env)))))

;;; Expression
(define-type Expression
  (U Const Op Not IsZero If Var Let Minus Proc TraceProc App LetRec))

(struct Const
  ([n : Real])
  #:transparent)

(struct Op
  ([op : Symbol]
   [n1 : Expression]
   [n2 : Expression])
  #:transparent)

(struct IsZero
  ([n : Expression])
  #:transparent)

(struct Not
  ([b : Expression])
  #:transparent)

(struct If
  ([test : Expression]
   [then : Expression]
   [else : Expression])
  #:transparent)

(struct Var
  ([v : Symbol])
  #:transparent)

(struct Let
  ([var : (Listof Symbol)]
   [val : (Listof Expression)]
   [body : Expression])
  #:transparent)

;; add minus by 3-6, hidaris
(struct Minus
  ([n : Expression])
  #:transparent)

(struct Proc
  ([param : (Listof Symbol)]
   [body  : Expression])
  #:transparent)

(struct TraceProc
  ([param : (Listof Symbol)]
   [body  : Expression])
  #:transparent)

(struct App
  ([proc : Expression]
   [arg  : (Listof Expression)])
  #:transparent)

(struct LetRec
  ([pnames    : (Listof Symbol)]
   [vars-lst  : (Listof (Listof Symbol))]
   [pbodys    : (Listof Expression)]
   [lbody     : Expression])
  #:transparent)

;;; Value
(define-type Value
  (U Num
     Bool
     Closure))

(struct Num
  ([n : Real])
  #:transparent)

(struct Bool
  ([b : Boolean])
  #:transparent)

(struct Closure
  ([var  : (Listof Symbol)]
   [body : Expression]
   [env  : Environment]
   [trace : Boolean])
  #:transparent)

(define-type Program (U AProgram))

(struct AProgram
  ([e : Expression])
  #:transparent)
