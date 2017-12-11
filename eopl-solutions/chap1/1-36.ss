#lang eopl

;;; number-elements : List -> Listof(List(Int, SchemeVal))
(define number-elements
  (lambda (lst)
    (if (null? lst) '()
        (g (list 0 (car lst))
           (number-elements (cdr lst))))))

;;; G : Listof(Listof(Int, SchemeVal)) -> Listof(Listof(Int, SchemeVal))
(define g
  (lambda (ns lst)
    (letrec
        [(G (lambda (ns)
              (cons (+ (car ns) 1)
                    (cdr ns))))]
      (cons ns
            (map G lst)))))
