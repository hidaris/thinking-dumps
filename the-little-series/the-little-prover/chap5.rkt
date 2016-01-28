#lang dracula
;; Load the J-Bob language:
(include-book "j-bob-lang" :dir :teachpacks)
;; Load J-Bob, our little proof assistant:
(include-book "j-bob" :dir :teachpacks)
;; Think it over and over and over.

(defun memb? (xs)
  (if (atom xs)
      'nil
      (if (equal (car xs) '?)
          't
          (memb? (cdr xs)))))

(defun remb (xs)
  (if (atom xs)
      '()
      (if (equal (car xs) '?)
          (remb (cdr xs))
          (cons (car xs)
                (remb (cdr xs))))))

(dethm memb?/remb0 ()
  (equal (memb? (remb '())) 'nil))

;;; Insight: Rewrite from the Inside Out
;;; Rewrite an expression from the "inside" out, starting inside
;;; if answers, if elses, and function arguments. Simplify the
;;; arguments of a function application as much as possible, then
;;; use the Law of Defun to replace the application with the function's
;;; body. Rewrite if questions as necessary to use theorems that require
;;; premises. Proceed to outer expressions when inner expressions cannot
;;; be simplified.

(dethm memb?/remb1 (x1)
  (equal (memb?
          (remb (cons x1 '())))
         'nil))
