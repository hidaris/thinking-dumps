#lang eopl

(require "base.ss")

;;; subst : Sym x Sym x S-list -> S-list
;;; subst-in-s-exp : Sym x Sym x S-exp -> S-exp
(define subst
  (lambda (new old slist)
    (letrec
        ((subst-in-s-exp
          (lambda (sexp)
            (if (symbol? sexp)
                (if (eqv? old sexp) new sexp)
                (subst new old sexp)))))
      (map subst-in-s-exp slist))))

(equal?? (subst 'a 'b '((b c) (b () d)))
         '((a c) (a () d)))
