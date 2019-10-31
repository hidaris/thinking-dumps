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
      (simple-let-1 "(let x 3 in x)" 3)

      ;; make sure the body and rhs get evaluated
      (eval-let-body "(let x 3 in (- x 1))" 2)
      (eval-let-rhs "(let x (- 4 1) in (- x 1))" 2)

      ;; check nested let and shadowing
      (simple-nested-let "(let x 3 in (let y 4 in (- x y)))" -1)
      (check-shadowing-in-body "(let x 3 in (let x 4 in x))" 4)
      (check-shadowing-in-rhs "(let x 3 in (let x (- x 1) in x))" 2)

      (negative-exp "(- 33)" -33)

      (dynamic-binding-exp "(let ([a 3])
                             in (let ([p (proc (x) (- x a))]
                                      [a 5])
                                in (- a (p 2))))" 8)

      ;; (apply-exp "((proc (x) (- x 33)) 0)" -33)

      ;; (apply-multi-exp "((proc (x y) (- x y)) 1 0)" 1)

      ;; (curried-exp "(let f (proc (x) (proc (y) (- x y))) in ((f 4) 3))" 1)

      ;; (test4-exp "(let makemult
      ;;               (proc (maker)
      ;;                 (proc (x)
      ;;                   (if (zero? x)
      ;;                       0
      ;;                       (- ((maker maker) (- x 1)) -4))))
      ;;             in (let time4
      ;;                  (proc (x)
      ;;                    ((makemult makemult) x))
      ;;                in (time4 3)))" 12)

      ;; (fact-exp "(let makefact
      ;;               (proc (maker)
      ;;                 (proc (x)
      ;;                   (if (zero? x)
      ;;                       1
      ;;                       (* ((maker maker) (- x 1)) x))))
      ;;             in (let fact
      ;;                  (proc (x)
      ;;                    ((makefact makefact) x))
      ;;                in (fact 3)))" 6)

      ;; (times-exp "(let makemult
      ;;               (proc (maker)
      ;;                 (proc (x)
      ;;                   (proc (y)
      ;;                     (if (zero? x)
      ;;                         0
      ;;                         (- (((maker maker) (- x 1)) y) (- y))))))
      ;;             in (let times
      ;;                  (proc (x)
      ;;                    (proc (y)
      ;;                      (((makemult makemult) x) y)))
      ;;                in ((times 3) 4)))" 12)

      ;; (times-fact-exp "(let makemult
      ;;               (proc (maker)
      ;;                 (proc (x)
      ;;                   (proc (y)
      ;;                     (if (zero? x)
      ;;                         0
      ;;                         (- (((maker maker) (- x 1)) y) (- y))))))
      ;;             in (let times
      ;;                  (proc (x)
      ;;                    (proc (y)
      ;;                      (((makemult makemult) x) y)))
      ;;                in (let makefact
      ;;               (proc (maker)
      ;;                 (proc (x)
      ;;                   (if (zero? x)
      ;;                       1
      ;;                       ((times ((maker maker) (- x 1))) x))))
      ;;             in (let fact
      ;;                  (proc (x)
      ;;                    ((makefact makefact) x))
      ;;                in (fact 3)))))" 6)

      ;; (odd?-exp "(let makeodd?
      ;;               (proc (maker)
      ;;                 (proc (x)
      ;;                   (if (zero? x)
      ;;                       (not (zero? x))
      ;;                       (not ((maker maker) (- x 1))))))
      ;;             in (let odd?
      ;;                  (proc (x)
      ;;                    ((makeodd? makeodd?) x))
      ;;                in (odd? 3)))" #t)

      ;; (not-odd?-exp "(let makeodd?
      ;;               (proc (maker)
      ;;                 (proc (x)
      ;;                   (if (zero? x)
      ;;                       (not (zero? x))
      ;;                       (not ((maker maker) (- x 1))))))
      ;;             in (let odd?
      ;;                  (proc (x)
      ;;                    ((makeodd? makeodd?) x))
      ;;                in (odd? 4)))" #f)

      ;; (even?-exp "(let makeeven?
      ;;               (proc (maker)
      ;;                 (proc (x)
      ;;                   (if (zero? x)
      ;;                       (zero? x)
      ;;                       (not ((maker maker) (- x 1))))))
      ;;             in (let even?
      ;;                  (proc (x)
      ;;                    ((makeeven? makeeven?) x))
      ;;                in (even? 3)))" #f)

      ;; (not-even?-exp "(let makeeven?
      ;;               (proc (maker)
      ;;                 (proc (x)
      ;;                   (if (zero? x)
      ;;                       (zero? x)
      ;;                       (not ((maker maker) (- x 1))))))
      ;;             in (let even?
      ;;                  (proc (x)
      ;;                    ((makeeven? makeeven?) x))
      ;;                in (even? 4)))" #t)

      ;; (rec-exp "(let makerec
      ;;             (proc (f)
      ;;               (let d
      ;;                 (proc (x)
      ;;                   (proc (z)
      ;;                     ((f (x x)) z)))
      ;;                 in (proc (n)
      ;;                      ((f (d d)) n))))
      ;;           in (let maketimes4
      ;;                (proc (f)
      ;;                  (proc (x)
      ;;                    (if (zero? x)
      ;;                        0
      ;;                        (- (f (- x 1)) (- 4)))))
      ;;              in (let times4 (makerec maketimes4)
      ;;                   in (times4 3))))" 12)

      ;; (traceproc-exp "((traceproc (x y) (- x y)) 1 0)" 1)

      ))
