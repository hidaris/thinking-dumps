#lang typed/racket

(require "ast.rkt")
(provide (all-defined-out))

(: empty-nameless-env
   (-> NameLessEnv))
(define (empty-nameless-env) '())

(: extend-nameless-env
   (-> Value NameLessEnv NameLessEnv))
(define (extend-nameless-env val nameless-env)
  (cons val nameless-env))

(: apply-nameless-env
   (-> Symbol LexAddr NameLessEnv Value))
(define (apply-nameless-env a n nameless-env)
  (if (eqv? n -1)
      (report-unbound-var a)
      ((inst list-ref Value) nameless-env n)))

(define report-unbound-var
  (lambda (var)
    (error 'translation-of "unbound variable in code: ~s" var)))

(: init-env
   (-> NameLessEnv))
(define (init-env)
  (extend-nameless-env
   (Num 1)
   (extend-nameless-env
    (Num 5)
    (extend-nameless-env
     (Num 10)
     (empty-nameless-env)))))
