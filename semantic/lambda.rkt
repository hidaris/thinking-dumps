#lang racket/base
(require redex/reduction-semantics)


(define-language Λ
  [V ::= x (λ x M)]
  [M ::= (M M) V]
  [C ::= hole (V C) (C M)]
  [x ::= variable-not-otherwise-mentioned]
  #:binding-forms
  (λ x M #:refers-to x))

(define -->β
  (reduction-relation Λ
    [--> (in-hole C ((λ x M) V))
         (in-hole C (substitute M x V))]))

(define (step-->β t)
  (define t* (apply-reduction-relation -->β t))
  (if (and (not (null? t*)) (null? (cdr t*)))
    (car t*)
    (raise-user-error 'step-->β "expected 1 result, got ~a" t*)))


(alpha-equivalent? Λ
  (step-->β (term ((λ x (λ y x)) y)))
    (term (λ a y)))