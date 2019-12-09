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
  (cond
    [(null? pnames) env]
    [else (extend-env-rec
           (car pnames)
           (car bvar-lsts)
           (car pbodys)
           (extend-env-rec*
            (cdr pnames)
            (cdr bvar-lsts)
            (cdr pbodys)
            env))]))

(: print-env (-> Environment Void))
(define (print-env env)
  (match env
    [(empty-env) (printf "---------end\n\n")]
    [(extend-env var val saved-env)
     (begin (printf "env------------\n")
            (printf "| ~a:~a |\n" var val)
            (print-env saved-env))]
    [(extend-env-rec pname bvars body saved-env)
     (begin (printf "rec-env------------\n")
            (printf "| ~a:~a |\n" pname 'function)
            (print-env saved-env))]))

(: rebuild-env
   (-> Environment Environment))
(define (rebuild-env env)
  (match env
    [(empty-env) (empty-env)]
    [(extend-env var val saved-env)
     (extend-env var val (rebuild-env saved-env))]
    [(extend-env-rec pname bvars body saved-env)
     (let ([vec (make-vector 1)])
       (let ([new-env (extend-env pname vec (rebuild-env saved-env))])
         (vector-set! vec 0 (Closure bvars
                                     body
                                     new-env
                                     #f))
         new-env))]))

(: apply-env
   (-> Symbol Environment Value))
(define (apply-env var env)
  (match env
    [(empty-env)
     (error 'apply-env
            "var ~s doesn't bound to a value" var)]
    [(extend-env sym val saved-env)
     #:when (not (vector? val))
     (if (eqv? var sym)
         (cast val Value)
         (apply-env var saved-env))]
    [(extend-env pname vec saved-env)
     #:when (vector? vec)
     (if (eqv? var pname)
         (cast (vector-ref vec 0) Value)
         (apply-env pname saved-env))]))

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
