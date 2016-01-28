#lang racket
(require "mk.rkt")

;; Under the Hood

(define u (var 'u))
(define v (var 'v))
(define w (var 'w))
(define x (var 'x))
(define y (var 'y))
(define z (var 'z))

(define lhs car)
(define rhs cdr)

; a substitution does not
; contain any associations.
(define empty-s '())

(define walk
  (λ (v s)
    (cond
      [(var? v)
       (cond
         [(assq v s) =>
          (λ (a)
            (walk (rhs a) s))]
         [else v])]
      [else v])))

;; extends a substition.
(define ext-s
  (λ (x v s)
    (cons `(,x . ,v) s)))

(define unify
  (λ (v w s)
    (let ([v (walk v s)]
          [w (walk w s)])
      (cond
        [(eq? v w) s]
        [(var? v) (ext-s v w s)]
        [(var? w) (ext-s w v s)]
        [(and (pair? v) (pair? w))
         (cond
           [(unify (car v) (car w) s) =>
            (λ (s)
              (unify (cdr v) (cdr w) s))]
           [else #f])]
        [(equal? v w) s]
        [else #f]))))

(define walk*
  (λ (v s)
    (let ([v (walk v s)])
      (cond
        [(var? v) v]
        [(pair? v)
         (cons
          (walk* (car v) s)
          (walk* (cdr v) s))]
        [else v]))))

(define-syntax project
  (syntax-rules ()
    ((_ (x ...) g ...)
     (lambdag@ (s)
       (let ((x (walk* x s)) ...)
         ((all g ...) s))))))

(run* (q)
  (== #f q)
  (project (q)
    (== (not (not q)) q)))
; => '(#f)

;; confused result.
(run* (q)
  ;; (== #f q)
  (fresh (q)
    (== (not (not q)) q)))
; => '(#f)

(define reify-name
  (lambda (n)
    (string->symbol
     (string-append "_" "." (number->string n)))))

(define reify-s
  (lambda (v s)
    (let ([v (walk v s)])
      (cond
        [(var? v)
         (ext-s v (reify-name (size-s s)) s)]
        [(pair? v) (reify-s (cdr v)
                            (reify-s (car v) s))]
        [else s]))))

(let ([r `(,w ,x ,y)])
  (walk* r (reify-s r empty-s)))
; => '(_.0 _.1 _.2)

(let ([r (walk* `(,x ,y ,z) empty-s)])
  (walk* r (reify-s r empty-s)))
; => '(_.0 _.1 _.2)

(let ([r `(,u (,v (,w ,x) ,y) ,x)])
  (walk* r (reify-s r empty-s)))
; => '(_.0 (_.1 (_.2 _.3) _.4) _.3)

(let ([s `((,y . (,z ,w c ,w)) (,x . ,y) (,z . a))])
  (let ([r (walk* x s)])
    (walk* r (reify-s r empty-s))))
; => '(a _.0 c _.0)

;; assumed that only arguments has been
;; walk*ed
(define reify
  (lambda (v)
    (walk* v (reify-s v empty-s))))

(let ([s `((,y . (,z ,w c ,w)) (,x . ,y) (,z . a))])
  (reify (walk* x s)))
; => '(a _.0 c _.0)

;; a new way to extend a substition.
;; why might we want to use ext-s-check
;; Because we might want to void creating
;; a circular substitution that if passed
;; to walk* might lead to no value.
(define ext-s-check
  (lambda (x v s)
    (cond
      [(occurs-check x v s) #f]
      [else (ext-s x v s)])))

;; e.g.
;; (==-check x y)
;; (occurs-check x y s) check do we have an occurrence
;; of x in y.
(define occurs-check
  (lambda (x v s)
    (let ([v (walk v s)])
      (cond
        [(var? v) (eq? v x)]
        [(pair? v)
         (or
          (occurs-check x (car v) s)
          (occurs-check x (cdr v) s))]
        [else #f]))))

(define unify-check
  (lambda (v w s)
    (let ([v (walk v s)]
          [w (walk w s)])
      (cond
        [(eq? v w) s]
        [(var? v) (ext-s-check v w s)]
        [(var? w) (ext-s-check w v s)]
        [(and (pair? v) (pair? w))
         (cond
           [(unify-check (car v) (car w) s) =>
            (λ (s)
              (unify-check (cdr v) (cdr w) s))]
           [else #f])]
        [(equal? v w) s]
        [else #f]))))

;; (run 1 (x)
;;   (== `(,x) x))
;; => It has no value

(run 1 (q)
  (fresh (x)
    (== `(,x) x)
    (== #t q)))
; => '(#t)

(run 1 (q)
  (fresh (x)
    (== `(,x) y)
    (== `(,y) x)
    (== #t q)))
; => '(#t)

(define ==-check
  (lambda (v w)
    (lambdag@ (s)
      (cond
        [(unify-check v w s) => succeed]
        [else (fail s)]))))

(run 1 (x)
  (==-check `(,x) x))
; => '()

;; (run 1 (x)
;;   (fresh (y z)
;;     (== x z)
;;     (== `(a b ,z) y)
;;     (== x y)))
; => has no value.

(run 1 (x)
  (fresh (y z)
    (== x z)
    (== `(a b ,z) y)
    (==-check x y)))
; => '()

;; when should we use ==-check
;; When we want to avoid creating a circular
;; substitution.

;; (run 1 (x)
;;   (== `(,x) x))
;; this generated the substitution.
;; `((,x . (,x))), which is a circular substition.
