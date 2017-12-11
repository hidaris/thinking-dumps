#lang eopl

;; the program will return (num-val 2),
;; because f = proc (a) (p 0) equal to proc (a) a
;; then eval with dynamic binding, a = 2
