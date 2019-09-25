#lang racket
(provide C 𝑪)
(require redex/reduction-semantics
         (only-in "../blackmail/semantics.rkt" B))

(define-extended-language C B
  (e ::= .... (if (zero? e) e e)))

(define-judgment-form C
  #:mode (𝑪 I O)
  #:contract (𝑪 e i)
  [----------
   (𝑪 i i)]

  [(𝑪 e_0 i_0) (where i_1 ,(+ (term i_0) 1))
   -----------
   (𝑪 (add1 e_0) i_1)]

  [(𝑪 e_0 i_0) (where i_1 ,(- (term i_0) 1))
   -----------
   (𝑪 (sub1 e_0) i_1)]

  [(𝑪 e_0 i_0) (side-condition ,(= (term i_0) 0)) (𝑪 e_1 i_1)
   --------
   (𝑪 (if (zero? e_0) e_1 e_2) i_1)]

  [(𝑪 e_0 i_0) (side-condition ,(!= (term i_0) 0)) (𝑪 e_2 i_2)
   --------
   (𝑪 (if (zero? e_0) e_1 e_2) i_2)])

(define (!= n1 n2)
  (not (= n1 n2)))
