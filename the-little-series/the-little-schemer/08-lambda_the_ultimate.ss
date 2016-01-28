(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? (car l) a)
      (cdr l))
     (else (cons (car l)
                 (rember-f test? a
                           (cdr l)))))))
(rember-f eq? 'a '(b c d a))
(rember-f equal? 2 '(3 4 2 3 2))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad
  (eq?-c 'salad))
(eq?-salad 'salad)

(define rember-f-c
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? (car l) a) (cdr l))
       (else (cons (car l)
                   ((rember-f-c test?) a (cdr l))))))))
((rember-f-c eq?) 'a '(b c a))

(define rember-eq? (rember-f-c eq?))

(rember-eq? 'a '(dd a e))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? old (car l))
        (cons new
              (cons old (cdr l))))
       (else (cons (car l)
                   ((insertL-f test?) new old
                    (cdr l))))))))
((insertL-f equal?) 'new '(a b c) '((a b c) e f))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? old (car l))
        (cons old
              (cons new (cdr l))))
       (else (cons (car l)
                   ((insertR-f test?) new old
                    (cdr l))))))))
((insertR-f equal?) 'new '(a b c) '((a b c) e f))

(define insert-g
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? old (car l))
        (cons new
              (cons old
                    (cons new
                          (cdr l)))))
       (else (cons (car l)
                   ((insert-g test?) new old
                    (cdr l))))))))
((insert-g equal?) 'new '(a b c) '((a b c) e f))

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))
(seqR 'a 'b '(a b c))

(define insert-g2
  (lambda (seq)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((equal? (car l) old)
        (seq new old (cdr l)))
       (else (cons (car l)
                   ((insert-g2 seq) new old
                    (cdr l))))))))

(define insertL2 (insert-g2 seqL))
(define insertR2 (insert-g2 seqR))

(insertL2 'a 'e '(c e d t d))

(define insertL3
  (insert-g2
   (lambda (new old l)
     (cons new (cons old l)))))

(define insertR3
  (insert-g2
   (lambda (new old l)
     (cons old (cons new l)))))

(define subst
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((equal? (car l) old)
      (cons new (cdr l)))
     (else (cons (car l)
                 (subst new old (cdr l)))))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define subst2 (insert-g2 seqS))

(define seqrem
  (lambda (new old l)
    l))

(define yyy
  (lambda (a l)
    ((insert-g2 seqrem) #f a l)))

(yyy 'sausage
     '(pizza with sausage and bacon))
;; the ninth commandment
;; abstract common patterns with a new function.

(define operator
  (lambda (aexp)
    (car aexp)))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

(define o+
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (o+ n (sub1 m)))))))

(define ox
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (o+ n (ox n (sub1 m)))))))

(define ^
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (ox n (^ n (sub1 m)))))))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     ((eq? (operator nexp)
           (quote +))
      (+ (value (1st-sub-exp nexp))
         (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp)
           (quote x))
      (ox (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
     (else
      (^ (value (1st-sub-exp nexp))
         (value (2nd-sub-exp nexp)))))))

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x (quote +)) o+)
     ((eq? x (quote x)) ox)
     (else ^))))

(define value2
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else
      ((atom-to-function
        (operator nexp))
       (value2 (1st-sub-exp nexp))
       (value2 (2nd-sub-exp nexp)))))))
(value2 '(+ 2 (^ 3 4)))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat))
      (multirember a (cdr lat)))
     (else (cons (car lat)
                 (multirember a (cdr lat)))))))
(multirember 'a '(a d a e r d a))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? a (car lat))
        ((multirember-f test?) a (cdr lat)))
       (else (cons (car lat)
                   ((multirember-f test?) a (cdr lat))))))))
((multirember-f eq?) 'a '(a d e a f a r))

(define multirember-eq?
  (multirember-f eq?))

(define eq?-tuna
  (eq?-c 'tuna))

(define multirember-f2
  (lambda (test?)
    (lambda (lat)
      (cond
       ((null? lat) '())
       ((test? (car lat))
        ((multirember-f2 test?) (cdr lat)))
       (else (cons (car lat)
                   ((multirember-f2 test?) (cdr lat))))))))
((multirember-f2 eq?-tuna) '(tuna b tuna e tuna))

(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) '())
     ((test? (car lat))
      (multiremberT test? (cdr lat)))
     (else (cons (car lat)
                 (multiremberT test?
                               (cdr lat)))))))
(multiremberT eq?-tuna '(tuna b tuan tuna d))

(define a-friend
  (lambda (x y)
    (null? y)))

(define late-friend
  (lambda (x y)
    (length x)))

(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat) (col '() '()))
     ((eq? a (car lat))
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col newlat
                             (cons (car lat) seen)))))
     (else
      (multirember&co a
                      (cdr lat)
                      (lambda (newlat seen)
                        (col (cons (car lat) newlat)
                             seen)))))))
(multirember&co 'tuna '(tuna dfs tuna erer tuna) late-friend)
;; The Tenth Commandment
;; Build functions to collect more than one value at a time.

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons new
            (cons old
                  (multiinsertL new old
                                (cdr lat)))))
     (else (cons (car lat)
                 (multiinsertL new old
                               (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) old)
      (cons old
            (cons new
                  (multiinsertR new old
                                (cdr lat)))))
     (else (cons (car lat)
                 (multiinsertR new old
                               (cdr lat)))))))

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? (car lat) oldL)
      (cons new
            (cons oldL
                  (multiinsertLR new oldL oldR
                                 (cdr lat)))))
     ((eq? (car lat) oldR)
      (cons oldR
            (cons new
                  (multiinsertLR new oldL oldR
                                 (cdr lat)))))
     (else
      (cons (car lat)
            (multiinsertLR new oldL oldR
                           (cdr lat)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat)
      (col (quote ()) 0 0))
     ((eq? (car lat) oldL)
      (multiinsertLR&co new oldL oldR
                        (cdr lat)
                        (lambda (newlat L R)
                          (col (cons new
                                     (cons oldL newlat))
                               (add1 L) R))))
     ((eq? (car lat) oldR)
      (multiinsertLR&co new oldL oldR
                        (cdr lat)
                        (lambda (newlat L R)
                          (col (cons oldR
                                     (cons new newlat))
                               L (add1 R)))))
     (else
      (multiinsertLR&co new oldL oldR
                        (cdr lat)
                        (lambda (newlat L R)
                          (col (cons (car lat) newlat)
                               L R)))))))
(multiinsertLR&co 'salty 'fish 'chips
                  '(chips and fish or fish and chips)
                  (lambda (newlat L R)
                    (length newlat)))

(define even2?
  (lambda (n)
    (= (* (quotient n 2) 2) n)))
(even2? 4)

(define evens-only*
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond ((even2? (car l))
             (cons (car l)
                   (evens-only* (cdr l))))
            (else (evens-only* (cdr l)))))
     (else (cons (evens-only* (car l))
                 (evens-only* (cdr l)))))))
(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

(define evens-only*&co
  (lambda (l col)
    (cond
     ((null? l) (col '() 1 0))
     ((atom? (car l))
      (cond ((even2? (car l))
             (evens-only*&co (cdr l)
                             (lambda (newl p s)
                               (col (cons (car l) newl)
                                    (* (car l) p) s))))
            (else (evens-only*&co (cdr l)
                                  (lambda (newl p s)
                                    (col newl
                                         p (+ (car l) s)))))))
     (else (evens-only*&co (car l)
                           (lambda (al ap as)
                             (evens-only*&co (cdr l)
                                             (lambda (dl dp ds)
                                               (col (cons al dl)
                                                    (* ap dp)
                                                    (+ as ds))))))))))
(define the-last-friend
  (lambda (newl product sum)
    (cons sum
          (cons product
                newl))))
(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)
