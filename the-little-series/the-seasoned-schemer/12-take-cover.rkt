#lang racket

(define multirember
  (lambda (a lat)
    (cond
     [(null? lat) (quote ())]
     [(eq? a (car lat))
      (multirember a (cdr lat))]
     [else (cons (car lat)
                 (multirember a (cdr lat)))])))

(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

;; it would be better if we did not have to
;; remind multirember for every natural recursion.
(define multirember-Y
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond
             ((null? lat) (quote ()))
             ((eq? a (car lat))
              (mr (cdr lat)))
             (else (cons (car lat)
                         (mr (cdr lat))))))))
     lat)))

;; (multirember 'a '(b a c a d))
(multirember-Y 'a '(b a c a d))

;; we do not use (define ...) to make length
;; recursive. Using Y on a function that looks
;; like length creates the recursive function.
(define length
  (Y (lambda (length)
       (lambda (l)
         (cond
          ((null? l) 0)
          (else
           (add1 (length (cdr l)))))))))
(length '(a v d))

;; So Y is a special version of (define ...)
;; but we also agreed that the definition with
;; (define ...) is easier to read than the definition
;; with Y.

(define multirember-letrec
  (lambda (a lat)
    ((letrec
         ([mr (lambda (lat)
                (cond
                 [(null? lat) (quote ())]
                 [(eq? a (car lat))
                  (mr (cdr lat))]
                 [else
                  (cons (car lat)
                        (mr (cdr lat)))]))])
       mr)
     lat)))

;; or can we write a recursive function mr?
(define mr
  (lambda (lat)
    (cond
     [(null? lat) (quote ())]
     [(eq? a (car lat))
      (mr (cdr lat))]
     [else
      (cons (car lat)
            (mr (cdr lat)))])))

;; this is wrong, since in multirember-mr, lambda(a lat), a and lat
;; could be anything else, so mr doesn't know a is in the environment.
(define multirember-mr
  (lambda (a lat)
    (mr lat)))
;; (multirember-mr 'a '(a v a))            ;=> can't refer undefined a

;; naming part: a function defined in the naming part of (letrec ...)
;; knows all the arguments of all the
;; surrounding (lambda ...) expressions.

;; the value part: it tells us what the result of the (letrec ...)
;; is. It may refer to the named recursive function.

;; this mean that (letrec ((mr ...)) mr)
;; defines and returns a recursive function
;; but that is a lot of parentheses for saying just that.

;; multirember-letrec had defined a function mr and had used it on lat.
;; and the good thing is that no other function can refer to mr.

;; the first line in (λ (a lat) ...) is now of the shape
;; (letrec ((mr ...)) (mr lat)), so multirember first defines
;; the recursive function mr that know about a.
;; and then the value part of (letrec ...) uses mr on lat,
;; so from here things proceed as before.
(define multirember-letrec2
  (lambda (a lat)
    (letrec
        ([mr (lambda (lat)
               (cond
                [(null? lat) (quote ())]
                [(eq? a (car lat))
                 (mr (cdr lat))]
                [else
                 (cons (car lat)
                       (mr (cdr lat)))]))])
      (mr lat))))

(multirember-letrec2 'a '(a c a))
;; (letrec ...) does is clear now, and it's better than Y.
;; the twelfth commandment
;; use (letrec ...) to remove arguments that do not
;; change for recursive applications.

;; rember can also remove numbers from a list
;; of numbers or S-expressions from a list of
;; S-expressions by the function rember-f, we
;; defined in The Little Schemer.
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       [(null? l) (quote ())]
       [(test? (car l) a)
        (cdr l)]
       [else (cons (car l)
                   ((rember-f test?) a
                    (cdr l)))]))))

;; Give a name to the function returned by (rember-f eq?)
;; and rember-eq? really rember
(define rember-eq? (rember-f eq?))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        [(null? lat) (quote ())]
       [(test? (car lat) a)
        ((multirember-f test?) a
         (cdr lat))]
       [else (cons (car lat)
                   ((multirember-f test?) a
                    (cdr lat)))]))))

;; we could define multirember-f-letrec with (letrec ...)
;; so that we don't need to re-determine the value of
;; (multirember-f test?)
(define multirember-f-letrec
  (lambda (test?)
    (letrec
        ([m-f
          (lambda (a lat)
            (cond
             [(null? lat) (quote ())]
             [(test? (car lat) a)
              (m-f a (cdr lat))]
             [else
              (cons (car lat)
                    (m-f a (cdr lat)))]))])
      m-f)))

(define multirember-letrec3
  (letrec
      ([mr (lambda (a lat)
             (cond
              [(null? lat) (quote ())]
              [(eq? (car lat) a)
               (mr a (cdr lat))]
              [else
               (cons (car lat)
                     (mr a (cdr lat)))]))])
    mr))

;; we coulde have used another name for the function named
;; in (letrec ...)
(define multirember-letrec4
  (letrec
      ((multirember-letrec4
        (lambda (a lat)
          (cond
           [(null? lat) (quote ())]
           [(eq? (car lat) a)
            (multirember-letrec4 a (cdr lat))]
           [else
            (cons (car lat)
                  (multirember-letrec4 a
                                       (cdr lat)))]))))
    multirember-letrec4))

;; since (letrec ...) defines a recursive function and
;; since (define ...) pairs up names with values, we could
;; eliminate (letrec ...) here.
;; we could and we would get back our old friend multirember.
(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a)
      (multirember a (cdr lat)))
     (else
      (cons (car lat)
            (multirember a (cdr lat)))))))

;; here is member? again:
(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     ((eq? (car lat) a) #t)
     (else (member? a (cdr lat))))))

;; here is one way of using (letrec ...)
(define member?-letrec
  (lambda (a lat)
    ((letrec
         ((yes? (lambda (l)
                  (cond
                   ((null? l) #f)
                   ((eq? (car l) a) #t)
                   (else (yes? (cdr l)))))))
       yes?)
     lat)))

;; here is an alternative:
(define member?-letrec2
  (lambda (a lat)
    (letrec
        ((yes? (lambda (l)
                 (cond
                  ((null? l) #f)
                  ((eq? (car l) a) #t)
                  (else (yes? (cdr l)))))))
      (yes? lat))))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2)
      (union (cdr set1) set2))
     (else (cons (car set1)
                 (union (cdr set1) set2))))))
(union '(a c b) '(b d e))

(define union-letrec
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond
               ((null? set) set2)
               ((member? (car set) set2)
                (U (cdr set)))
               (else (cons (car set)
                           (U (cdr set))))))))
      (U set1))))

;; we could also have written it like:
(define union-letrec1
  (lambda (set1 set2)
    (letrec
        ((A (lambda (set)
              (cond
               ((null? set) set2)
               ((member? (car set) set2)
                (A (cdr set)))
               (else (cons (car set)
                           (A (cdr set))))))))
      (A set1))))

;; why do we choose the name U,
;; to keep the boxes from getting too wide, we
;; use single letter names within (letrec ...)
;; for such minor functions.

(define union-letrec-almost
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond
               ((null? set) set2)
               ((member? (car set) set2);;(M? ...)
                (A (cdr set)))
               (else (cons (car set)
                           (A (cdr set)))))))
         (member?;; -> M?
          (lambda (a lat)
            (cond
             ((null? lat) #f)
             ((eq? (car lat) a) #t)
             (else (member? a (cdr lat)))))))
      (U set1))))
;; we get thirteenth commandment
;; use (letrec ...) to hide and to protect functions.

(define union-letrec-final
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set)
              (cond
               ((null? set) set2)
               ((M? (car set) set2)
                (U (cdr set)))
               (else (cons (car set)
                           (U (cdr set)))))))
         (M? (lambda (a lat)
               (letrec
                   ((N? (lambda (lat)
                          (cond
                           ((null? lat) #f)
                           ((eq? (car lat) a) #t)
                           (else (N? (cdr lat)))))))
                 (N? lat)))))
      (U set1))))
(union-letrec-final '(a c b) '(b d e))

(define two-in-a-row-letrec?
  (lambda (lat)
    (letrec
        ((W (lambda (a lat)
              (cond
               ((null? lat) #f)
               (else (or (eq? (car lat) a)
                         (W (car lat)
                            (cdr lat))))))))
      (cond
       ((null? lat) #f)
       (else (W (car lat) (cdr lat)))))))

(define two-in-a-row-letrec2?
  (letrec
      ((W (lambda (a lat)
            (cond
             ((null? lat) #f)
             (else (or (eq? (car lat) a)
                       (W (car lat)
                          (cdr lat))))))))
    (lambda (lat)
      (cond
       ((null? lat) #f)
       (else (W (car lat) (cdr lat)))))))

(define sum-of-prefixes-letrec
  (lambda (tup)
    (letrec
        ((S (lambda (sss tup)
              (cond
               ((null? tup) (quote ()))
               (else
                (cons (+ sss (car tup))
                      (S (+ sss (car tup))
                         (cdr tup))))))))
      (S 0 tup))))

(define scramble-letrec
  (lambda (tup)
    (letrec
        ((P (lambda (tup rp)
              (cond
               ((null? tup) (quote ()))
               (else (cons (pick (car tup)
                                 (cons (car tup) rp))
                           (P (cdr tup)
                              (cons (car tup) rp)))))))
         (pick (lambda (n lat)
                 (cond
                  ((one? n) (car lat))
                  (else (pick (sub1 n) (cdr lat))))))
         (one? (lambda (n)
                 (zero? (sub1 n)))))
      (P tup (quote ())))))
