;; b = (bool-val #t), (expval->num (num-val b)) != b
;; due to expval->num, num-val is undefined for expressed value bool-val
