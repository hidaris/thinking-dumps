#lang racket

(provide test-list)

  ;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;

  (define test-list
    '(

      ;; simple arithmetic
      (positive-const "11" 11)
      (negative-const "-33" -33)
      (simple-arith-1 "(- 44 33)" 11)

      ;; nested arithmetic
      (nested-arith-left "(- (- 44 33) 22)" -11)
      (nested-arith-right "(- 55 (- 22 11))" 44)

      ;; simple variables
      (test-var-1 "x" 10)
      (test-var-2 "(- x 1)" 9)
      (test-var-3 "(- 1 x)" -9)

      ;; simple unbound variables
      (test-unbound-var-1 "foo" error)
      (test-unbound-var-2 "(- x foo)" error)

      ;; simple conditionals
      (if-true "(if (zero? 0) 3 4)" 3)
      (if-false "(if (zero? 1) 3 4)" 4)

      ;; test dynamic typechecking
      (no-bool-to-diff-1 "(- (zero? 0) 1)" error)
      (no-bool-to-diff-2 "(- 1 (zero? 0))" error)
      (no-int-to-if "(if 1 2 3)" error)

      ;; make sure that the test and both arms get evaluated
      ;; properly.
      (if-eval-test-true "(if (zero? (- 11 11)) 3 4)" 3)
      (if-eval-test-false "(if (zero? (- 11 12)) 3 4)" 4)

      ;; and make sure the other arm doesn't get evaluated.
      (if-eval-test-true-2 "(if (zero? (- 11 11)) 3 foo)" 3)
      (if-eval-test-false-2 "(if (zero? (- 11 12)) foo 4)" 4)

      ;; simple let
      (simple-let-1 "(let ([x 3]) in x)" 3)

      ;; make sure the body and rhs get evaluated
      (eval-let-body "(let ([x 3]) in (- x 1))" 2)
      (eval-let-rhs "(let ([x (- 4 1)]) in (- x 1))" 2)

      ;; check nested let and shadowing
      (simple-nested-let "(let ([x 3]) in (let ([y 4]) in (- x y)))" -1)
      (check-shadowing-in-body "(let ([x 3]) in (let ([x 4]) in x))" 4)
      (check-shadowing-in-rhs "(let ([x 3]) in (let ([x (- x 1)]) in x))" 2)

      (negative-exp "(- 33)" -33)

      (dynamic-binding-exp "(let ([a 3])
                             in (let ([p (proc (x) (- x a))]
                                      [a 5])
                                in (- a (p 2))))" 8)

      ;; simple letrecs
      (simple-letrec-1 "(letrec (f x) = (- x 1) in (f 33))" 32)
      (simple-letrec-2
       "(letrec (f x) = (if (zero? x) 0 (- (f (- x 1)) -2)) in (f 4))"
       8)

      (simple-letrec-3
       "(let ([m -5])
 in (letrec (f x) = (if (zero? x) 0 (- (f (- x 1)) m)) in (f 4))))"
       20)

      (simple-letrec-4 "(letrec (f x y) = (- x y) in (f 33 1))" 32)

      ;; alas, no multiplication in this language.  Exercise: define
      ;; multiplication as a letrec and then use it to define factorial.
      ;;      (fact-of-6  "letrec
      ;;  fact(x) = if zero?(x) then 1 else *(x, (fact sub1(x)))
      ;;  in (fact 6)"
      ;;                  720)

      (HO-nested-letrecs
       "(letrec (even odd) = (proc (x) (if (zero? x) 1 (odd (- x 1))))
   in (letrec (odd x) = (if (zero? x) 0 ((even odd) (- x 1)))
   in (odd 13))))" 1)

      ))
