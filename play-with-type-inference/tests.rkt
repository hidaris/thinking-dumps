#lang racket

(require rackunit "tt-minikanren.rkt")

(define-syntax inhabits
  (λ (stx)
    (syntax-case stx ()
      [(inhabits e τ)
       #'(let ([msg (format "~a :: ~a" e τ)])
           (begin
             (displayln msg)
             (check member τ (run* (q) (Ͱ '() e q)) msg)
             (check member e (run 100 (q) (Ͱ '() q τ)) msg)))]
      [(inhabits e τ e′)
       #'(let ([msg (format "~a :: ~a" e τ)])
           (begin
             (displayln msg)
             (check member τ (run* (q) (Ͱ '() e q)) msg)
             (if (eqv? e′ #f)
                 (void)
                 (check member e′ (run 100 (q) (Ͱ '() q τ)) msg))))])))

(define ⊢tests
  (test-suite
   "Tests for ⊢"
   (inhabits '⊤ '⊤)
   (inhabits '(λ (x) x) '(_.0 -> _.0)
             '((λ (_.0) _.0) (sym _.0)))
   (inhabits '((λ (x) x) (λ (y) y)) '(_.0 -> _.0)
             '(((λ (_.0) _.0) (λ (_.1) _.1)) (sym _.0 _.1)))
   (inhabits '(λ (x) ⊤) '(_.0 -> ⊤)
             '((λ (_.0) ⊤) (sym _.0)))
   (inhabits '(inl ⊤) '(⊤ + _.0))
   (inhabits '(inr ⊤) '(_.0 + ⊤))
   (inhabits '(match (inl ⊤) ((inl a) ⊤) ((inr b) ⊤)) '⊤ #f)
   (inhabits '(cons ⊤ ⊤) '(⊤ × ⊤))
   (inhabits '(car (cons ⊤ ⊤)) '⊤)
   (inhabits '(cdr (cons ⊤ ⊤)) '⊤)
   (inhabits '(λ (x) (λ (y) (y x))) '(_.0 -> ((_.0 -> _.1) -> _.1))
             '((λ (_.0) (λ (_.1) (_.1 _.0))) (=/= ((_.0 _.1))) (sym _.0 _.1)))
   (inhabits '(λ (v) (cons (match v
                             ((inl ac) (inl (car ac)))
                             ((inr bc) (inr (car bc))))
                           (match v
                             ((inl ac) (cdr ac))
                             ((inr bc) (cdr bc)))))
             '(((_.0 × _.1) + (_.2 × _.1)) -> ((_.0 + _.2) × _.1))
             '((λ (_.0) (cons (match _.0
                                ((inl _.1) (inl (car _.1)))
                                ((inr _.2) (inr (car _.2))))
                              (match _.0
                                ((inl _.3) (cdr _.3))
                                ((inr _.4) (cdr _.4)))))
               (sym _.0 _.1 _.2 _.3 _.4)))
   (inhabits '(λ (e)
                (match e
                  ((inl ab) (match ab
                              ((inl a) (inl a))
                              ((inr b) (inr (inl b)))))
                  ((inr c) (inr (inr c)))))
             '(((_.0 + _.1) + _.2) -> (_.0 + (_.1 + _.2)))
             #f)))

(require rackunit/text-ui)
(run-tests ⊢tests)
