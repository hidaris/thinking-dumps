#lang racket

(provide (all-defined-out))

(define succeed (lambdag@ (s) (unit s)))
(define fail (lambdag@ (s) (mzero)))

(define caro
  (lambda (p a)
    (fresh (d)
      (== (cons a d) p))))

(define cdro
  (lambda (p d)
    (fresh (a)
      (== (cons a d) p))))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

(define nullo
  (lambda (x)
    (== '() x)))

(define eqo
  (lambda (x y)
    (== x y)))

(define eq-caro
  (lambda (l x)
    (caro l x)))

(define pairo
  (lambda (p)
    (fresh (a d)
      (conso a d p))))

(define listo
  (lambda (l)
    (conde
      ((nullo l) succeed)
      ((pairo l)
       (fresh (d)
         (cdro l d)
         (listo d)))
      (else fail))))

(define membero
  (lambda (x l)
    (conde
      ((nullo l) fail)
      ((eq-caro l x) succeed)
      (else
       (fresh (d)
         (cdro l d)
         (membero x d))))))

(define rembero
  (lambda (x l out)
    (conde
      ((nullo l) (== '() out))
      ((eq-caro l x) (cdro l out))
      (else (fresh (a d res)
              (conso a d l)
              (rembero x d res)
              (conso a res out))))))

(define appendo
  (lambda (l s out)
    (conde
      ((nullo l) (== s out))
      (else
       (fresh (a d res)
         (conso a d l)
         (conso a res out)
         (appendo d s res))))))

(define anyo
  (lambda (g)
    (conde
      (g succeed)
      (else (anyo g)))))

(define nevero (anyo fail))

(define alwayso (anyo succeed))

(define build-num
  (lambda (n)
    (cond
      ((zero? n) '())
      ((and (not (zero? n)) (even? n))
       (cons 0
             (build-num (quotient n 2))))
      ((odd? n)
       (cons 1
             (build-num (quotient (- n 1) 2)))))))

(define full-addero
  (lambda (b x y r c)
    (conde
      ((== 0 b) (== 0 x) (== 0 y) (== 0 r) (== 0 c))
      ((== 1 b) (== 0 x) (== 0 y) (== 1 r) (== 0 c))
      ((== 0 b) (== 1 x) (== 0 y) (== 1 r) (== 0 c))
      ((== 1 b) (== 1 x) (== 0 y) (== 0 r) (== 1 c))
      ((== 0 b) (== 0 x) (== 1 y) (== 1 r) (== 0 c))
      ((== 1 b) (== 0 x) (== 1 y) (== 0 r) (== 1 c))
      ((== 0 b) (== 1 x) (== 1 y) (== 0 r) (== 1 c))
      ((== 1 b) (== 1 x) (== 1 y) (== 1 r) (== 1 c))
      (else fail))))

(define poso
  (lambda (n)
    (fresh (a d)
      (== `(,a . ,d) n))))

(define >1o
  (lambda (n)
    (fresh (a ad dd)
      (== `(,a ,ad . ,dd) n))))

(define addero
  (lambda (d n m r)
    (condi
     ((== 0 d) (== '() m) (== n r))
     ((== 0 d) (== '() n) (== m r)
      (poso m))
     ((== 1 d) (== '() m)
      (addero 0 n '(1) r))
     ((== 1 d) (== '() n) (poso m)
      (addero 0 '(1) m r))
     ((== '(1) n) (== '(1) m)
      (fresh (a c)
        (== `(,a ,c) r)
        (full-addero d 1 1 a c)))
     ((== '(1) n) (gen-addero d n m r))
     ((== '(1) m) (>1o n) (>1o r)
      (addero d '(1) n r))
     ((>1o n) (gen-addero d n m r))
     (else fail))))

(define gen-addero
  (lambda (d n m r)
    (fresh (a b c e x y z)
      (== `(,a . ,x) n)
      (== `(,b . ,y) m) (poso y)
      (== `(,c . ,z) r) (poso z)
      (alli
       (full-addero d a b c e)
       (addero e x y z)))))

(define +o
  (lambda (n m k)
    (addero 0 n m k)))

(define -o
  (lambda (n m k)
    (+o m k n)))

(define *o
  (lambda (n m p)
    (condi
     ((== '() n) (== '() p))
     ((poso n) (== '() m) (== '() p))
     ((== '(1) n) (poso m) (== m p))
     ((>1o n) (== '(1) m) (== n p))
     ((fresh (x z)
        (== `(0 . ,x) n) (poso x)
        (== `(0 . ,z) p) (poso z)
        (>1o m)
        (*o x m z)))
     ((fresh (x y)
        (== `(1 . ,x) n) (poso x)
        (== `(0 . ,y) m) (poso y)
        (*o m n p)))
     ((fresh (x y)
        (== `(1 . ,x) n) (poso x)
        (== `(1 . ,y) m) (poso y)
        (odd-*o x n m p)))
     (else fail))))

(define odd-*o
  (lambda (x n m p)
    (fresh (q)
      (bound-*o q p n m)
      (*o x m q)
      (+o `(0 . ,q) m p))))

(define bound-*o
  (lambda (q p n m)
    (conde
      ((nullo q) (pairo p))
      (else
       (fresh (x y z)
         (cdro q x)
         (cdro p y)
         (condi
          ((nullo n)
           (cdro m z)
           (bound-*o x y z '()))
          (else
           (cdro n z)
           (bound-*o x y z m))))))))

(define =lo
  (lambda (n m)
    (conde
      ((== '() n) (== '() m))
      ((== '(1) n) (== '(1) m))
      (else
       (fresh (a x b y)
         (== `(,a . ,x) n) (poso x)
         (== `(,b . ,y) m) (poso y)
         (=lo x y))))))

(define <lo
  (lambda (n m)
    (conde
      ((== '() n) (poso m))
      ((== '(1) n) (>1o m))
      (else
       (fresh (a x b y)
         (== `(,a . ,x) n) (poso x)
         (== `(,b . ,y) m) (poso y)
         (<lo x y))))))

(define <=lo
  (lambda (n m)
    (condi
     ((=lo n m) succeed)
     ((<lo n m) succeed)
     (else fail))))

(define <o
  (lambda (n m)
    (condi
     ((<lo n m) succeed)
     ((=lo n m)
      (fresh (x)
        (poso x)
        (+o n x m)))
     (else fail))))

(define <=o
  (lambda (n m)
    (condi
     ((== n m) succeed)
     ((<o n m) succeed)
     (else fail))))

(define /o
  (lambda (n m q r)
    (condi
     ((== r n) (== '() q) (<o n m))
     ((== '(1) q) (=lo n m) (+o r m n)
      (<o r m))
     (else
      (alli
       (<lo m n)
       (<o r m)
       (poso q)
       (fresh (nh nl qh ql qlm qlmr rr rh)
         (alli
          (splito n r nl nh)
          (splito q r ql qh)
          (conde
            ((== '() nh)
             (== '() qh)
             (-o nl r qlm)
             (*o ql m qlm))
            (else
             (alli
              (poso nh)
              (*o ql m qlm)
              (+o qlm r qlmr)
              (-o qlmr nl rr)
              (splito rr r '() rh)
              (/o nh m qh rh)))))))))))

(define splito
  (lambda (n r l h)
    (condi
     ((== '() n) (== '() h) (== '() l))
     ((fresh (b n^)
        (== `(0 ,b . ,n^) n)
        (== '() r)
        (== `(,b . ,n^) h)
        (== '() l)))
     ((fresh (n^)
        (==  `(1 . ,n^) n)
        (== '() r)
        (== n^ h)
        (== '(1) l)))
     ((fresh (b n^ a r^)
        (== `(0 ,b . ,n^) n)
        (== `(,a . ,r^) r)
        (== '() l)
        (splito `(,b . ,n^) r^ '() h)))
     ((fresh (n^ a r^)
        (== `(1 . ,n^) n)
        (== `(,a . ,r^) r)
        (== '(1) l)
        (splito n^ r^ '() h)))
     ((fresh (b n^ a r^ l^)
        (== `(,b . ,n^) n)
        (== `(,a . ,r^) r)
        (== `(,b . ,l^) l)
        (poso l^)
        (splito n^ r^ l^ h)))
     (else fail))))

(define logo
  (lambda (n b q r)
    (condi
     ((== '(1) n) (poso b) (== '() q) (== '() r))
     ((== '() q) (<o n b) (+o r '(1) n))
     ((== '(1) q) (>1o b) (=lo n b) (+o r b n))
     ((== '(1) b) (poso q) (+o r '(1) n))
     ((== '() b) (poso q) (== r n))
     ((== '(0 1) b)
      (fresh (a ad dd)
        (poso dd)
        (== `(,a ,ad . ,dd) n)
        (exp2 n '() q)
        (fresh (s)
          (splito n dd r s))))
     ((fresh (a ad add ddd)
        (conde
          ((== '(1 1) b))
          (else (== `(,a ,ad ,add . ,ddd) b))))
      (<lo b n)
      (fresh (bw1 bw nw nw1 ql1 ql s)
        (exp2 b '() bw1)
        (+o bw1 '(1) bw)
        (<lo q n)
        (fresh (q1 bwq1)
          (+o q '(1) q1)
          (*o bw q1 bwq1)
          (<o nw1 bwq1))
        (exp2 n '() nw1)
        (+o nw1 '(1) nw)
        (/o nw bw ql1 s)
        (+o ql '(1) ql1)
        (conde
          ((== q ql))
          (else (<lo ql q)))
        (fresh (bql qh s qdh qd)
          (repeated-mul b ql bql)
          (/o nw bw1 qh s)
          (+o ql qdh qh)
          (+o ql qd q)
          (conde
            ((== qd qdh))
            (else (<o qd qdh)))
          (fresh (bqd bq1 bq)
            (repeated-mul b qd bqd)
            (*o bql bqd bq)
            (*o b bq bq1)
            (+o bq r n)
            (<o n bq1)))))
     (else fail))))

(define exp2
  (lambda (n b q)
    (condi
     ((== '(1) n) (== '() q))
     ((>1o n) (== '(1) q)
      (fresh (s)
        (splito n b s '(1))))
     ((fresh (q1 b2)
        (alli
         (== `(0 . ,q1) q)
         (poso q1)
         (<lo b n)
         (appendo b `(1 . ,b) b2)
         (exp2 n b2 q1))))
     ((fresh (q1 nh b2 s)
        (alli
         (== `(1 . ,q1) q)
         (poso q1)
         (poso nh)
         (splito n b s nh)
         (appendo b `(1 . ,b) b2)
         (exp2 nh b2 q1))))
     (else fail))))

(define repeated-mul
  (lambda (n q nq)
    (conde
      ((poso n) (== '() q) (== '(1) nq))
      ((== '(1) q) (== n nq))
      ((>1o q)
       (fresh (q1 nq1)
         (+o q1 '(1) q)
         (repeated-mul n q1 nq1)
         (*o nq1 n nq)))
      (else fail))))

(define expo
  (lambda (b q n)
    (logo n b q '())))

;;;  'trace-vars' can be used to print the values of selected variables
;;;  in the substitution.
(define-syntax trace-vars
  (syntax-rules ()
    ((_ title x ...)
     (lambdag@ (s)
       (begin
         (printf "~a~n" title)
         (for-each (lambda (x_ t)
                     (printf "~a = ~s~n" x_ t))
                   `(x ...) (reify (walk* `(,x ...) s)))
         (unit s))))))

;;; (run* (q)
;;;   (fresh (r)
;;;     (== 3 q)
;;;     (trace-vars "What it is!" q r)))
;;;
;;; What it is!
;;; q = 3
;;; r = _.0
;;; (3)

(define-syntax lambdag@
  (syntax-rules ()
    ((_ (s) e) (lambda (s) e))))

(define-syntax lambdaf@
  (syntax-rules ()
    ((_ () e) (lambda () e))))

(define-syntax rhs
  (syntax-rules ()
    ((rhs p) (cdr p))))

(define-syntax lhs
  (syntax-rules ()
    ((lhs p) (car p))))

(define-syntax var
  (syntax-rules ()
    ((var w) (vector w))))

(define-syntax var?
  (syntax-rules ()
    ((var? w) (vector? w))))

(define-syntax size-s
  (syntax-rules ()
    ((size-s ls) (length ls))))

(define empty-s '())

(define walk
  (lambda (v s)
    (cond
      ((var? v)
       (cond
         ((assq v s) =>
          (lambda (a)
            (let ((v^ (rhs a)))
              (walk v^ s))))
         (else v)))
      (else v))))

(define ext-s
  (lambda (x v s)
    (cons `(,x . ,v) s)))

(define unify
  (lambda (v w s)
    (let ((v (walk v s))
          (w (walk w s)))
      (cond
        ((eq? v w) s)
        ((var? v) (ext-s v w s))
        ((var? w) (ext-s w v s))
        ((and (pair? v) (pair? w))
         (cond
           ((unify (car v) (car w) s) =>
            (lambda (s)
              (unify (cdr v) (cdr w) s)))
           (else #f)))
        ((equal? v w) s)
        (else #f)))))

(define ext-s-check
  (lambda (x v s)
    (cond
      ((occurs-check x v s) #f)
      (else (ext-s x v s)))))

(define occurs-check
  (lambda (x v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) (eq? v x))
        ((pair? v)
         (or
          (occurs-check x (car v) s)
          (occurs-check x (cdr v) s)))
        (else #f)))))

(define unify-check
  (lambda (v w s)
    (let ((v (walk v s))
          (w (walk w s)))
      (cond
        ((eq? v w) s)
        ((var? v) (ext-s-check v w s))
        ((var? w) (ext-s-check w v s))
        ((and (pair? v) (pair? w))
         (cond
           ((unify-check (car v) (car w) s) =>
            (lambda (s)
              (unify-check (cdr v) (cdr w) s)))
           (else #f)))
        ((equal? v w) s)
        (else #f)))))

(define walk*
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) v)
        ((pair? v)
         (cons
          (walk* (car v) s)
          (walk* (cdr v) s)))
        (else v)))))

(define reify-s
  (lambda (v s)
    (let ((v (walk v s)))
      (cond
        ((var? v) (ext-s v (reify-name (size-s s)) s))
        ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
        (else s)))))

(define reify-name
  (lambda (n)
    (string->symbol
     (string-append "_" "." (number->string n)))))

(define reify
  (lambda (v)
    (walk* v (reify-s v empty-s))))

(define-syntax run
  (syntax-rules ()
    ((_ n^ (x) g ...)
     (let ((n n^) (x (var 'x)))
       (if (or (not n) (> n 0))
           (map-inf n
             (lambda (s) (reify (walk* x s)))
             ((all g ...) empty-s))
           '())))))

(define-syntax case-inf
  (syntax-rules ()
    ((_ e on-zero ((a^) on-one) ((a f) on-choice))
     (let ((a-inf e))
       (cond
         ((not a-inf) on-zero)
         ((not (and
                (pair? a-inf)
                (procedure? (cdr a-inf))))
          (let ((a^ a-inf))
            on-one))
         (else (let ((a (car a-inf))
                     (f (cdr a-inf)))
                 on-choice)))))))

(define-syntax mzero
  (syntax-rules ()
    ((_) #f)))

(define-syntax unit
  (syntax-rules ()
    ((_ a) a)))

(define-syntax choice
  (syntax-rules ()
    ((_ a f) (cons a f))))

(define map-inf
  (lambda (n p a-inf)
    (case-inf a-inf
      '()
      ((a)
       (cons (p a) '()))
      ((a f)
       (cons (p a)
             (cond
               ((not n) (map-inf n p (f)))
               ((> n 1) (map-inf (- n 1) p (f)))
               (else '())))))))

(define ==
  (lambda (v w)
    (lambdag@ (s)
      (cond
        ((unify v w s) => succeed)
        (else (fail s))))))

(define ==-check
  (lambda (v w)
    (lambdag@ (s)
      (cond
        ((unify-check v w s) => succeed)
        (else (fail s))))))

(define-syntax fresh
  (syntax-rules ()
    ((_ (x ...) g ...)
     (lambdag@ (s)
       (let ((x (var 'x)) ...)
         ((all g ...) s))))))

(define-syntax all
  (syntax-rules ()
    ((_) succeed)
    ((_ g) (lambdag@ (s) (g s)))
    ((_ g^ g ...) (lambdag@ (s) (bind (g^ s) (all g ...))))))

(define-syntax conde
  (syntax-rules (else)
    ((_) fail)
    ((_ (else g0 g ...)) (all g0 g ...))
    ((_ (g0 g ...) c ...)
     (anye (all g0 g ...) (conde c ...)))))


(define bind
  (lambda (a-inf g)
    (case-inf a-inf
      (mzero)
      ((a) (g a))
      ((a f) (mplus (g a)
               (lambdaf@ () (bind (f) g)))))))

(define mplus
  (lambda (a-inf f)
    (case-inf a-inf
      (f)
      ((a) (choice a f))
      ((a f0) (choice a
                (lambdaf@ () (mplus (f0) f)))))))

(define-syntax anye
  (syntax-rules ()
    ((_ g1 g2)
     (lambdag@ (s)
       (mplus (g1 s)
         (lambdaf@ () (g2 s)))))))

(define-syntax alli
  (syntax-rules ()
    ((_) succeed)
    ((_ g) (lambdag@ (s) (g s)))
    ((_ g^ g ...)
     (lambdag@ (s)
       (bindi (g^ s) (alli g ...))))))

(define-syntax condi
  (syntax-rules (else)
    ((_) fail)
    ((_ (else g0 g ...)) (all g0 g ...))
    ((_ (g0 g ...) c ...)
     (anyi (all g0 g ...) (condi c ...)))))

(define-syntax anyi
  (syntax-rules ()
    ((_ g1 g2)
     (lambdag@ (s)
       (mplusi (g1 s)
         (lambdaf@ () (g2 s)))))))

(define bindi
  (lambda (a-inf g)
    (case-inf a-inf
      (mzero)
      ((a) (g a))
      ((a f) (mplusi (g a)
               (lambdaf@ () (bindi (f) g)))))))

(define mplusi
  (lambda (a-inf f)
    (case-inf a-inf
      (f)
      ((a) (choice a f))
      ((a f0) (choice a
                (lambdaf@ () (mplusi (f) f0)))))))

(define-syntax conda
  (syntax-rules (else)
    ((_) fail)
    ((_ (else g0 g ...)) (all g0 g ...))
    ((_ (g0 g ...) c ...)
     (ifa g0 (all g ...) (conda c ...)))))

(define-syntax condu
  (syntax-rules (else)
    ((_) fail)
    ((_ (else g0 g ...)) (all g0 g ...))
    ((_ (g0 g ...) c ...)
     (ifu g0 (all g ...) (condu c ...)))))

(define-syntax ifa
  (syntax-rules ()
    ((_ g0 g1 g2)
     (lambdag@ (s)
       (let ((s-inf (g0 s)) (g^ g1))
         (case-inf s-inf
           (g2 s)
           ((s) (g^ s))
           ((s f) (bind s-inf g^))))))))

(define-syntax ifu
  (syntax-rules ()
    ((_ g0 g1 g2)
     (lambdag@ (s)
       (let ((s-inf (g0 s)) (g^ g1))
         (case-inf s-inf
           (g2 s)
           ((s) (g^ s))
           ((s f) (g^ s))))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x) g ...) (run #f (x) g ...))))

(define-syntax lambda-limited
  (syntax-rules ()
    ((_ n formals g)
     (let ((x (var 'x)))
       (lambda formals
         (ll n x g))))))

(define ll
  (lambda (n x g)
    (lambdag@ (s)
      (let ((v (walk x s)))
        (cond
          ((var? v) (g (ext-s x 1 s)))
          ((< v n) (g (ext-s x (+ v 1) s)))
          (else (fail s)))))))

(define-syntax project
  (syntax-rules ()
    ((_ (x ...) g ...)
     (lambdag@ (s)
       (let ((x (walk* x s)) ...)
         ((all g ...) s))))))
