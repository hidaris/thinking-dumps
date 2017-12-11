#lang eopl

;;; NodeInSequence ::= (Int Listof(Int) Listof(Int))
;;; number-sequence : Int -> (Int Listof(Int) Listof(Int))
(define number-sequence
  (lambda (n)
    (list n '() '())))

;;; current-element : sequence -> Int
(define current-element
  (lambda (s)
    (car s)))

;;; move-to-left : sequence -> sequence
(define move-to-left
  (lambda (s)
    (if (at-left-end? s)
        (eopl:error 'move-to-left
                    "already at the left end")
        (let ((num (list-ref s 0))
              (left (list-ref s 1)))
          (list (car left)
                (cdr left)
                (cons num
                      (list-ref s 2)))))))

;;; move-to-right : sequence -> sequence
(define move-to-right
  (lambda (s)
    (if (at-right-end? s)
        (eopl:error 'move-to-right
                    "already at the right end")
        (let ((num (list-ref s 0))
              (right (list-ref s 2)))
          (list (car right)
                (cons num
                      (list-ref s 1))
                (cdr right))))))

;;; insert-to-left : Int x sequence -> sequence
(define insert-to-left
  (lambda (n s)
    (list (list-ref s 0)
          (cons n (list-ref s 1))
          (list-ref s 2))))

;;; insert-to-right : Int x sequence -> sequence
(define insert-to-right
  (lambda (n s)
    (list (list-ref s 0)
          (list-ref s 1)
          (cons n (list-ref s 2)))))

;;; at-left-end? : Int x sequence -> Bool
(define at-left-end?
  (lambda (s)
         (null? (list-ref s 1))))

;;; at-right-end? : Int x sequence -> Bool
(define at-right-end?
  (lambda (s)
         (null? (list-ref s 2))))

