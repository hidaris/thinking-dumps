#lang typed/racket

(provide (all-defined-out))

(require/typed "parser.rkt"
  [parse (-> String Program)])
(require "ast.rkt")
(require "env.rkt")
(require "translator.rkt")

(: value-of-cond
   (-> (Listof Exp)
       (Listof Exp)
       NameLessEnv
       Value))
(define value-of-cond
  (lambda (lefts rights env)
    (cond
      [(null? lefts) (error 'cond "No left-hand is true")]
      [(val->bool (value-of (car lefts) env))
       (value-of (car rights) env)]
      [else
       (value-of-cond (cdr lefts) (cdr rights) env)])))

(: value-of (-> Exp NameLessEnv Value))
(define (value-of exp env)
  (match exp
    [(Const n) (Num n)]
    [(NameLessVar a n) (apply-nameless-env a n env)]
    [(Diff n1 n2)
     (let ([val1 (value-of n1 env)]
           [val2 (value-of n2 env)])
       (let ([sval1 (val->num val1)]
             [sval2 (val->num val2)])
         (Num (- sval1 sval2))))]
    [(IsZero n)
     (let ([val (value-of n env)])
       (let ([sval (val->num val)])
         (if (zero? sval)
             (Bool #t)
             (Bool #f))))]
    [(If test then else)
     (let ([test-val (value-of test env)])
       (if (val->bool test-val)
           (value-of then env)
           (value-of else env)))]
    [(NameLessLet exp body)
     (let ([val (value-of exp env)])
       (value-of body (extend-nameless-env val env)))]
    [(NameLessProc body)
     (Closure body env)]
    [(App proc arg)
     (let ([v1 (value-of proc env)]
           [v2 (value-of arg env)])
       (apply-procedure (val->closure v1) v2))]
    [(Minus n)
     (let ([val (value-of n env)])
       (let ([sval (val->num val)])
         (Num (- sval))))]
    [(Cond test-lst answer-lst)
     (value-of-cond test-lst answer-lst env)]))

(: apply-procedure (Closure Value -> Value))
(define (apply-procedure proc val)
  (match proc
    [(Closure body saved-env)
     (value-of body (extend-nameless-env val saved-env))]))

(: value-of-program (-> Program Value))
(define (value-of-program pgm)
  (match pgm
    ([AProgram exp1]
     (value-of exp1 (init-env)))))

(: value-of-translation (-> Program Value))
(define (value-of-translation pgm)
  (match pgm
    ([AProgram exp1]
     (value-of exp1 (init-env)))))

(: run (-> String Value))
(define run
  (λ (str)
    (value-of-translation
     (translation-of-program (parse str)))))
