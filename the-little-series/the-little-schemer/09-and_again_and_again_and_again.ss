(define atom?
  (lambda (a)
    (and (not (pair? a)) (not (null? a)))))

(define pick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))
(pick 3 '(a b c d))

(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((number? sorn)
      (keep-looking a (pick sorn lat) lat))
     (else (eq? sorn a)))))

;; Functions like looking are called partial functions.
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))
(looking 'caviar '(6 2 4 caviar 5 7 3))

;; a shorter function that does not reach its goal
;; for some of its arguments.
(define eternity
  (lambda (x)
    (eternity x)))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define build
  (lambda (s1 s2)
    (cons s1
          (cons s2
                '()))))

(define shift
  (lambda (p)
    (build (first (first p))
           (build (second (first p))
                  (second p)))))
(shift '((a (d c)) (a d e)))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f)
     ((null? (cdr (cdr x))) #t)
     (else #f))))

;; align yields a value for every argument.
(define align
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (align (shift pora)))
     (else (build (first pora)
                  (align (second pora)))))))
(align '((a d c) (b e r)))
(align '((a d) (e f)))

;; both the result and the argument of shift have the same number of atoms
;; write a function that counts the number of atoms in align's arguments.
(define length*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (+ (length* (first pora))
         (length* (second pora)))))))
(length* '((a b) (d e)))

(define weight*
  (lambda (pora)
    (cond
     ((atom? pora) 1)
     (else
      (+ (* (weight* (first pora)) 2)
         (weight* (second pora)))))))
(weight* '((a b) c))
(weight* '(a (b c)))

;; helper
(define revpair
  (lambda (pair)
    (build (second pair)
           (first pair))))

;; the function shuffle is not total because it
;; now swaps the components of the pair again,
;; which means that we start all over.
;; like '((a b) (c b))
(define my-shuffle
  (lambda (pora)
    (cond
     ((atom? pora) pora)
     ((a-pair? (first pora))
      (my-shuffle (revpair pora)))
     (else (build (first pora)
                  (my-shuffle (second pora)))))))
(my-shuffle '(a (b c)))

(define one?
  (lambda (n)
    (= n 1)))

;; C is also partial functions
;; because it doesn't yield a value for 0.
(define C
  (lambda (n)
    (cond
     ((one? n) 1)
     (else
      (cond
       ((even? n) (C (quotient n 2)))
       (else (C (add1 (* 3 n)))))))))
(C 4)

(define A
  (lambda (n m)
    (cond
     ((zero? n) (add1 m))
     ((zero? m) (A (sub1 n) 1))
     (else (A (sub1 n)
              (A n (sub1 m)))))))
(A 1 1)
(A 2 2)
;; will have decayed long before we could possibly
;; have calculated the value of (A 4 3)

;; we can write a function that tells us whether some function
;; return with a value for every argument

;; we have seen functions that never return a value
;; or return a value so late that it is too late,
;; we should have some tool like this around.

;; for a warm-up exercise, let's focus on a function that checks
;; whether some function stops for just the empty list, the simplest
;; of all arguments.

(define will-stop?
  (lambda (f)
    ...))
(will-stop? length)   ;; #t (length '()) => 0
(will-stop? eternity) ;; #f (eternity '()) => doesn't return a value

;; one more example.
(define last-try
  (lambda (x)
    (and (will-stop? last-try)
         (eternity x))))
;; this is interesting, if we want the value of (last-try '()),
;; we must determine the value of
;; (and (will-stop? last-try) (eternity '()))
;; that depends on the value of (will-stop? last-try)
;; there are only two possibilities. let's say (will-stop? last-try) is #f
;; then (and #f (eternity '())) is #f, since (and #f ...) is always #f
;; so (last-try '()) stopped.
;; but (will-stop? last-try) was #f, which really means that last-try will
;; not stop.
;; so we must have been wrong about (will-stop? last-try), it must return #t
;; we think will-stop? always give #t or #f, it was total.
;; if (will-stop? last-try) return #t, the value of (last-try '())
;; is the same as the value of (eternity '()).
;; but (eternity '()) doesn't return a value. so it doesn't stop.
;; so this mean that will-stop? cannot be defined.

;; it is unique, it makes will-stop? the first function that we can describe
;; but cannot define in our language.

;; length0
(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 (eternity (cdr l))))))

;; but (define ...) doesn't work for length0
;; so, replace length0 by its definition
(lambda (l)
  (cond
   ((null? l) 0)
   (else
    (add1
     ((lambda (l)
        (cond
         ((null? l) 0)
         (else (add1
                (eternity (cdr l))))))
      (cdr l))))))

;; a good name for this length<=1
(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 (length0 (cdr l))))))

(lambda (l)
  (cond
   ((null? l) 0)
   (else
    (add1
     ((lambda (l)
        (cond
         ((null? l) 0)
         (else
          (add1
           ((lambda (l)
              (cond
               ((null? l) 0)
               (else
                (add1
                 (eternity
                  (cdr l))))))
            (cdr l))))))
      (cdr l))))))

;; rewrite length0
((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
 eternity)

;; rewrite length<=1
((lambda (f)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (f (cdr l)))))))
 ((lambda (g)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (g (cdr l)))))))
  eternity))

;; rewrite length<=2
((lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
 ((lambda (length)
    (lambda (l)
      (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
  ((lambda (length)
     (lambda (l)
       (cond
        ((null? l) 0)
        (else (add1 (length (cdr l)))))))
   eternity)))

;; name the function that takes length as an argument
;; and that returns a function that looks like length.
((lambda (mk-length)
   (mk-length eternity))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

;; length<=1
((lambda (mk-length)
   (mk-length
    (mk-length eternity)))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

;; length<=2
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length eternity))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

;; okay, here is length<=3
((lambda (mk-length)
   (mk-length
    (mk-length
     (mk-length
      (mk-length eternity)))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

;; what is recursion like?
;; It is like an infinite tower of applications of
;; mk-length to an arbitrary function.

;; 在length0中, eternity并未执行，
;; so, replace eternity with mk-length
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

;; 'a, length -> mk-length, length0 will be:
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (mk-length (cdr l))))))))

;; length<=1
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 ((mk-length eternity) (cdr l))))))))

;; length<=1 展开
(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 (((lambda (mk-length)
                   (lambda (l)
                     (cond
                      ((null? l) 0)
                      (else (add1 ((mk-length eternity) (cdr l)))))))
                 eternity) (cdr l))))))

;; (mk-length eternity)只针对 length<=1，当 length > 1时，我们需要函数可以自动展开
;; 将 eternity 替换为 mk-length, 我们可以在 cdr 不为空链表时， 继续执行else中的展开
(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 ((mk-length mk-length) (cdr l))))))
;; |
;; |
;; v
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 ((mk-length mk-length) (cdr l))))))))

(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 (((lambda (mk-length)
                   (lambda (l)
                     (cond
                      ((null? l) 0)
                      (else (add1 ((mk-length mk-length) (cdr l)))))))
                 mk-length) (cdr l))))))

(lambda (l)
  (cond
   ((null? l) 0)
   (else (add1 ((lambda (l)
                  (cond
                   ((null? l) 0)
                   (else (add1 (((lambda (mk-length)
                                   (lambda (l)
                                     (cond
                                      ((null? l) 0)
                                      (else (add1 ((mk-length mk-length) (cdr l)))))))
                                 mk-length) (cdr l))))))
                (cdr l))))))

(define length
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (add1 (length (cdr l)))))))

;; compare with
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 ((mk-length mk-length) (cdr l))))))))
;; 提取 (mk-length mk-length)
;; |
;; |
;; v
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))

;; 将一致的部分提到外层
((lambda (le)
   ((lambda (mk-length)
      (mk-length mk-length))
    (lambda (mk-length)
      (le (lambda (x)
            ((mk-length mk-length) x))))))
 (lambda (length)
   (lambda (l)
     (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))))

;; 我们得到了一个额外的东西
(lambda (le)
  ((lambda (mk-length)
     (mk-length mk-length))
   (lambda (mk-length)
     (le (lambda (x)
           ((mk-length mk-length) x))))))
;; 可以将非递归函数转化为递归形式

;; it is called applicative-order Y combinator
(define Y
  (lambda (le)
    ((lambda (f)
       (f f))
     (lambda (f)
       (le (lambda (x)
             ((f f) x)))))))

;; now that we know what recursion is.
