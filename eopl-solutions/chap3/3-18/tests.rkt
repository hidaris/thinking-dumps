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

      (let-muti-exp "(let ((a 1) (b 2) (c 3)) in (- a c))" -2)
      (let*-exp "(let* ([x 2] [y (- x 1)]) in (- y 1))" 0)
      (unpack-exp "(unpack (([a b] (list 2 3))) in (- b a))" 1)
      ))
