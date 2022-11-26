#lang racket

;; create a unique var
;; Each use of var creates a new one-element
;; vector representing a unique variable. We ignore the
;; vectors’ contents, instead distinguishing vectors by
;; their addresses in memory. We could instead
;; distinguish variables by their values, provided we
;; ensure their values are unique (for example, using a
;; unique natural number in each variable).
(define (var name) (vector name))
(define (var? x) (vector? x))

(define empty-s '())

(define (walk v s)
  (let ([a (and (var? v) (assv v s))])
    (cond
      [(pair? a) (walk (cdr a) s)]
      [else v])))

;; whether or not x occurs in v, using s
;; avoid cycle substitution
(define (occurs? x v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) (eqv? x v)] ;; `((v . ,x))
      [(pair? v)
       (or (occurs? x (car v) s)
           (occurs? x (cdr v) s))]
      [else #f])))

(define (ext-s x v s)
  (cond
    [(occurs? x v s) #f]
    [else (cons `(,x . ,v) s)]))

(define (unify u v s)
  (let ([u (walk u s)]
        [v (walk v s)])
    (cond
      [(eqv? u v) s]
      [(var? u) (ext-s u v s)]
      [(var? v) (ext-s v u s)]
      [(and (pair? u) (pair? v))
       (let ([s (unify (car u) (car v) s)])
         (and s
              (unify (cdr u) (cdr v) s)))]
      [else #f])))

(define (== u v)
  (lambda (s)
    (let ([s (unify u v s)])
      (if s `(,s) '()))))

(define succeed
  (lambda (s)
    `(,s)))

(define fail
  (lambda (s)
    '()))

(define (disj2 g1 g2)
  (lambda (s)
    (append$ (g1 s) (g2 s))))

(define (append$ s$ t$)
  (cond
    [(null? s$) t$]
    [(pair? s$)
     (cons (car s$)
           (append$ (cdr s$) t$))]
    [else (lambda ()
            (append$ t$ (s$)))]))

(define (nevero)
  (lambda (s)
    (lambda ()
      ((nevero) s))))

;; (let ([s-inf ((disj2
;;                (== 'olive (var 'x))
;;                (nevero))
;;               empty-s)])
;;   s-inf)

;; (let ([s-inf ((disj2
;;                (nevero)
;;                (== 'olive (var 'x)))
;;               empty-s)])
;;   (s-inf))
(define (alwayso)
  (lambda (s)
    (lambda ()
      ((disj2 succeed (alwayso)) s))))

;; (let ([s-inf (((alwayso) empty-s))])
;;   (cons (car s-inf)
;;         (let ([s-inf ((cdr s-inf))])
;;           (cons (car s-inf)
;;                 (let ([s-inf ((cdr s-inf))])
;;                   (cons (car s-inf) '()))))))
(define (take$ n s$)
  (cond
    [(and n (zero? n)) '()]
    [(null? s$) '()]
    [(pair? s$)
     (cons (car s$)
           (take$ (and n (sub1 n))
                  (cdr s$)))]
    [else (take$ n (s$))]))

;; (let ([k (length (take$ 5 ((disj2 (== 'olive (var 'x))
;;                                   (== 'oil (var 'x)))
;;                            empty-s)))])
;;   `(Found ,k not 5 substitutions))

(define (append-map$ g s$)
  (cond
    [(null? s$) '()]
    [(pair? s$)
     (append$ (g (car s$))
                 (append-map$ g (cdr s$)))]
    [else (lambda ()
            (append-map$ g (s$)))]))

(define (conj2 g1 g2)
  (lambda (s)
    (append-map$ g2 (g1 s))))

(define (call/fresh name f)
  (f (var name)))

(define (reify-name n)
  (string->symbol
   (string-append "_"
                  (number->string n))))

(define (walk* v s)
  (let ([v (walk v s)])
    (cond
      [(var? v) v]
      [(pair? v)
       (cons
        (walk* (car v) s)
        (walk* (cdr v) s))]
      [else v])))

(define (reify-s v r)
  (let ([v (walk v r)])
    (cond
      [(var? v)
       (let ([n (length r)])
         (let ([rn (reify-name n)])
           (cons `(,v . ,rn) r)))]
      [(pair? v)
       (let ([r (reify-s (car v) r)])
         (reify-s (cdr v) r))]
      [else r])))

(define (reify v)
  (lambda (s)
    (let ([v (walk* v s)])
      (let ([r (reify-s v empty-s)])
        (walk* v r)))))

(define (run-goal n g)
  (take$ n (g empty-s)))
