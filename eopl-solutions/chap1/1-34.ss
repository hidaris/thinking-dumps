#lang eopl
(require "base.ss")

;;; path : Int x bst -> List
;;; usage: (path n bst) returns a list of lefts and rights showing how to find
;;;        node containing n. If n is found at the root, it returns the empty
;;;        list.
(define path
  (lambda (n bst)
    (letrec
        [(P (lambda (n-bst)
              (cond
                [(null? n-bst)
                 (eopl:error 'path "~s not found in ~s.~%" n bst)]
                [(eqv? n (contents-of n-bst)) '()]
                [(< n (contents-of n-bst))
                 (cons 'left (P (lson n-bst)))]
                [else
                 (cons 'right (P (rson n-bst)))])))
         (lson (lambda (n)
                 (cadr n)))
         (rson (lambda (n)
                 (caddr n)))
         (contents-of
          (lambda (n)
            (if (integer? n)
                n
                (car n))))]
      (P bst))))

(equal?? (path 17 '(14 (7 () (12 () ()))
                       (26 (20 (17 () ())
                               ())
                           (31 () ()))))
         '(right left left))

(equal?? (path 17 '(17 (7 () ())
                       (26 () ())))
         '())

(equal?? (path 12 '(13 (10 () ())
                       (14 () ())))
         '())
