(define atom?
  (lambda (a)
    (and (not (null? a)) (not (pair? a)))))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (= a1 a2))
     ((or (number? a1) (number? a2))
      #f)
     (else (eq? a1 a2)))))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2))
      #t)
     ((or (null? l1) (null? l2))
      #f)
     (else (and (equal? (car l1) (car l2))
                (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eqan? s1 s2))
     ((or (atom? s1) (atom? s2)) #f)
     (else (eqlist? s1 s2)))))

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     ((equal? a (car lat)) #t)
     (else
      (member? a (cdr lat))))))

(define my-set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (my-set? (cdr lat))))))

(my-set? '(apple 3 pear 4 9 apple 3 4))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat))
      (makeset (cdr lat)))
     (else (cons (car lat)
                 (makeset (cdr lat)))))))
(makeset '(apple peach pear peach
                 plum apple lemon peach))

(define multirember
  (lambda (a l)
    (cond
     ((null? l) (quote ()))
     ((equal? a (car l))
      (multirember a (cdr l)))
     (else (cons (car l)
                 (multirember a (cdr l)))))))
(multirember 'a
             '(b a c a))

(define makeset2
  (lambda (lat)
    (cond
     ((null? lat) (quote ()))
     (else (cons (car lat)
                 (makeset2
                  (multirember (car lat)
                               (cdr lat))))))))
(makeset2 '(apple peach pear peach plum apple lemon peach))

(define my-subset?
  (lambda (s1 s2)
    (cond
     ((null? s1) #t)
     (else
      (and (member? (car s1) s2)
           (my-subset? (cdr s1) s2))))))

(my-subset? '(1 1 2)
            '(1 2))

(define eqset?
  (lambda (s1 s2)
    (and (my-subset? s1 s2)
         (my-subset? s2 s1))))

(eqset? '((a) 3)
        '((a) 3))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else
      (or (member? (car set1) set2)
          (intersect?
           (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (cons (car set1)
            (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(intersect '(a b e c)
           '(b a d c))


(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else (cons (car set1)
                 (union (cdr set1) set2))))))

(define xxx
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2)
      (xxx (cdr set1) set2))
     (else (cons (car set1)
                 (xxx (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set)
                      (intersectall (cdr l-set)))))))
(intersectall '((a b) (b d) (e b)))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))
(a-pair? '((a (b)) (b c d)))

(define (first p)
  (car p))

(define (second p)
  (car (cdr p)))

(define (build s1 s2)
  (cons s1
        (cons s2 '())))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l))
                 (firsts (cdr l)))))))

(define fun?
  (lambda (rel)
    (my-set? (firsts rel))))

(define revpair
  (lambda (pair)
    (build (second pair)
           (first pair))))

(define revrel
  (lambda (rel)
    (cond
     ((null? rel) '())
     (else (cons (revpair (car rel))
                 (revrel (cdr rel)))))))

(define seconds
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (cdr (car l))
                 (seconds (cdr l)))))))
(seconds '((a b) (d e) (j f)))

(define fullfun?
  (lambda (fun)
    (my-set? (seconds fun))))
(fullfun? '((grape raisin) (plum prune) (stewed prune)))

(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))
