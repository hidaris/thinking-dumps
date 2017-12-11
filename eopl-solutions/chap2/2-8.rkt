#lang eopl
;;; A data-structure representation of environments

;;; Env = (empty-env) | (extend-env Var SchemeVal Env)
;;; Var = Sym

;;; empty-env : () -> Env
(define empty-env
  (lambda ()'()))

;;; empty-env? : Env -> Bool
(define empty-env?
  (lambda (env)
    (null? env)))
