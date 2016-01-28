;; True or false: (lat? l)
;; where
;;   l is (Jack Sprat could eat no chicken fat)     True, because each S-expression in l is an atom.
;; True or false: (lat? l)
;; where
;;   l is ((Jack) Sprat could eat no chicken fat)   False, since (car l) is a list.
;; True or false: (lat? l)
;; where
;;   l is (Jack (Sprat could) eat no chicken fat)   False, since one of the S-expressions in l is a list.
;; True or false: (lat? l)
;; where l is ()                                    True, because it does not contain a list.
;; True or false: a lat is a list of atoms.         True! Every lat is a list of atomS!
;; Write the function lat? using some,
;; but not necessarily all of the following,functions: car cdr cons null? atom? and eq?;
(define atom?
  (lambda (l)
    (and (not (null? l)) (not (pair? l)))))
(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))
(lat? '(bacon and eggs))                ;#t
(lat? '(bacon (and eggs)))              ;#f

(or (null? '()) (atom? '(d e f g)))     ;#t '() is null list
(or (null? '(a b c)) (null? '()))       ;#t '() is null
(or (null? '(a b c)) (null? '(atom)))   ;#f all not true

;; (define member?
;;   (lambda (a lat)
;;     (cond
;;      ((null? lat) #f)
;;      (else (or (eq? (car lat) a)
;;                (member? a (cdr lat)))))))
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     ((eq? (car lat) a) #t)
     (else (member? a (cdr lat))))))
(member? 'd '(a b c))
(member? 'bc '(ab bc cd))
(member? '(ab) '((bc) (ab)))            ;#f eq? accept non-empty atom
;; The First Commandment
;; Always ask null? as the first question in expressing any function.
(member? 'liver '(bagels and lox))      ;#f
