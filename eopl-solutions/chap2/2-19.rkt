#lang eopl

;;; Bintree := () | (Int Bintree Bintree)

;;; number->bintree : Int -> (Int Bintree Bintree)
(define number->bintree
  (lambda (n)
    (list n '() '())))

;;; insert-to-right : Int x Bintree -> Bintree
(define insert-to-right
  (lambda (n bt)
    (list (list-ref bt 0)
          (list-ref bt 1)
          (let ((node-right (list-ref bt 2)))
            (if (at-leaf? node-right)
                (number->bintree n)
                (list n
                      '()
                      node-right))))))

;;; insert-to-left : Int x Bintree -> Bintree
(define insert-to-left
  (lambda (n bt)
    (list (list-ref bt 0)
          (let ((node-left (list-ref bt 1)))
            (if (at-leaf? node-left)
                (number->bintree n)
                (list n
                      node-left
                      '())))
          (list-ref bt 2))))

;;; move-to-left : bintree -> bintree
(define move-to-left
  (lambda (bt)
    (list-ref bt 1)))

;;; move-to-right : bintree -> bintree
(define move-to-right
  (lambda (bt)
    (list-ref bt 2)))

;;; current-element : bintree -> Int
(define current-element
  (lambda (bt)
    (list-ref bt 0)))

;;; at-leaf? : bintree -> bool
(define at-leaf?
  (lambda (bt)
    (null? bt)))
