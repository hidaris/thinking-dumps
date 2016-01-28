;;
(define atom?
  (lambda (a)
    (and (not (null? a)) (not (pair? a)))))

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

(define o-
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (o- n (sub1 m)))))))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2))
      (= a1 a2))
     ((or (number? a1) (number? a2))
      #f)
     (else (eq? a1 a2)))))

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) a)
        (rember* a (cdr l)))
       (else (cons (car l)
                   (rember* a (cdr l))))))
     (else (cons (rember* a (car l))
                 (rember* a (cdr l)))))))
(rember* 'cup
         '((tom cup) cup
           (fly cup)))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons old
              (cons new
                    (insertR* new old
                              (cdr l)))))
       (else (cons (car l)
                   (insertR* new old
                             (cdr l))))))
     (else (cons (insertR* new old
                           (car l))
                 (insertR* new old
                           (cdr l)))))))
(insertR* 'new 'old
          '((old add)
            old
            (app old (old a))
            (((old)))))

(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l))
      (cond
       ((eq? a (car l))
        (add1 (occur* a (cdr l))))
       (else (occur* a (cdr l)))))
     (else (o+ (occur* a (car l))
               (occur* a (cdr l)))))))

(occur* 'a
        '((a a) b
          (b (a (b))) a
          ((c a))))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? old (car l))
        (cons new
              (subst* new old (cdr l))))
       (else (cons (car l)
                   (subst* new old
                           (cdr l))))))
     (else
      (cons (subst* new old (car l))
            (subst* new old (cdr l)))))))

(subst* 'a 'b
        '((a b)
          ((b b d) (k c b))
          ((c b (b))) b))

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l))
      (cond
       ((eq? (car l) old)
        (cons new
              (cons old
                    (insertL* new old
                              (cdr l)))))
       (else (cons (car l)
                   (insertL* new old
                             (cdr l))))))
     (else (cons (insertL* new old
                           (car l))
                 (insertL* new old
                           (cdr l)))))))

(insertL* 'b 'a
          '((a c d (c a c))
            (k a ((j a))) a))

(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l))
      (or (eq? (car l) a)
          (member* a (cdr l))))
     (else (or (member* a (car l))
               (member* a (cdr l)))))))

(member* 'chips
         '((potato) (chips ((with a) chips))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(leftmost '((((a ()) c) e) d))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l1) (null? l2)) #f)
     (else
      (and (equal? (car l1) (car l2))
           (eqlist? (cdr l1) (cdr l2)))))))
(eqlist? '(1 3 4 5)
         '(1 3 4 d))

(define equall?
  (lambda (s1 s2)
    (cond
     ((and (atom? s1) (atom? s2))
      (eqan? s1 s2))
     ((or (atom? s1) (atom? s2))
      #f)
     (else (eqlist? s1 s2)))))
(equall? '((a) b)
         '((a) b))
(equall? 1 2)
;;simplify only after function is correct.

(define rember
  (lambda (s l)
    (cond
     ((null? l) '())
     ((equall? (car l) s) (cdr l))
     (else (cons (car l)
                 (rember s
                         (cdr l)))))))

(rember '(s d)
        '((s d) (s d) f))
