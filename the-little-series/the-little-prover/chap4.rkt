#lang racket/base

;; Load the J-Bob language:
(require "j-bob/j-bob-lang.rkt")
;; Load J-Bob, our little proof assistant:
(require "j-bob/j-bob.rkt")
;; to part of this total breakfast.

(defun my-list0? (x)
  (if (equal x 'oatmeal)
      'nil
      (if (equal x '())
          't
          (if (equal x '(toast))
              'nil
              'nil))))

(defun my-list0?₂ (x)
  (equal x '()))

(defun list1? (x)
  (if (atom x)
      'nil
      (list0? (cdr x))))

(defun list2?₁ (x)
  (if (atom x)
      'nil
      (list1? (cdr x))))

;;; The Law of Defun (final)
;;; Given the total function (defun name (x1 ... xn) body),
;;; (name e1 ... en) = body where x1 is e1, ..., xn is en.

;;; Here is a partial functions.
(defun partial (x)
  (if (partial x)
      'nil
      't))

(dethm contradiction ()
  'nil)

(defun list? (x)
  (if (atom x)
      (equal x '())
      (list? (cdr x))))

;;; A measure is an expression that is
;;; included with a function definition. It
;;; may only refer to previously defined,
;;; total functions and to the function
;;; definition's formal arguments. The measure must
;;; produce a natural number that decreases for every
;;; recursive call to the function.

;;; The Axioms of Size
(dethm natp/size (x)
  (equal (natp (size x)) 't))

(dethm size/car (x)
  (if (atom x) 't (equal (< (size (car x)) (size x)) 't)))

(dethm size/cdr (x)
  (if (atom x) 't (equal (< (size (cdr x)) (size x)) 't)))

(defun sub (x y)
  (if (atom y)
      (if (equal y '?)
          x
          y)
      (cons (sub x (car y))
            (sub x (cdr y)))))
