#lang racket

;; The identity monad
(define mybegin
  (λ (x f)
    (f x)))

(mybegin (printf "One\n")
  (λ (_) (printf "Two\n")))

(mybegin (printf "One\n")
  (λ (_) (mybegin (printf "Two\n")
            (λ (_) (printf "Three\n")))))

(define mylet
  (λ (x f)
    (f x)))

(mylet 5 (λ (x) (+ x 3)))
(mylet 5 (λ (x) (mylet x (λ (y) (+ x y)))))

;; ma -> (a -> mb) -> mb
(define bind_identity
  (λ (ma sequel)
    (sequel ma))) ; <= This is a mb.

;; a -> ma
(define unit_identity
  (λ (a)
    a)) ; <= This is a ma.

(bind_identity
    (unit_identity (printf "One\n"))
    (λ (_)
      (bind_identity
       (unit_identity (printf "Two\n"))
       (λ (_)
         (unit_identity (printf "Three\n"))))))

;; ma -> (a -> mb) -> mb
(bind_identity
 (unit_identity 5)
  (λ (x) (unit_identity (+ x 3))))

(bind_identity
 (unit_identity 5)
 (λ (x)
   (bind_identity
    (unit_identity x)
    (λ (y) (unit_identity (+ x y))))))

;; The State Monad
(define even-length?
  (λ (ls)
    (cond
      [(null? ls) #t]
      [else
       (not (even-length? (cdr ls)))])))

(define even-length?_sps
  (λ (ls s)
    (cond
      [(null? ls) s]
      [else
       (even-length?_sps (cdr ls) (not s))])))

(define even-length?_state
  (λ (ls)
    (cond
      [(null? ls) (unit_state '_)]
      [else (bind_state
              (λ (s)
                `(_ . ,(not s)))
              (λ (_)
                (even-length?_state (cdr ls))))])))

(define unit_state
  (λ (a)
    (λ (s) ; <= This function is a ma.
      `(,a . ,s))))

(define bind_state
  (λ (ma sequel)
    (λ (s) ; <= This function is a mb.
      (let ((p (ma s)))
        (let ((a_^ (car p))
              (s_^ (cdr p)))
          (let ((mb (sequel a_^)))
            (mb s_^)))))))

(define remberevensXcountevens_2pass
  (λ (l)
    `(,(remberevens_direct l) . ,(countevens_direct l))))

(define remberevens_direct
  (λ (l)
    (cond
      [(null? l) `()]
      [(pair? (car l))
       (cons (remberevens_direct (car l))
             (remberevens_direct (cdr l)))]
      [(or (null? (car l)) (odd? (car l)))
       (cons (car l)
             (remberevens_direct (cdr l)))]
      [else (remberevens_direct (cdr l))])))

(define countevens_direct
  (λ (l)
    (cond
      [(null? l) 0]
      [(pair? (car l))
       (+ (countevens_direct (car l))
          (countevens_direct (cdr l)))]
      [(or (null? (car l)) (odd? (car l)))
       (countevens_direct (cdr l))]
      [else (add1 (countevens_direct (cdr l)))])))

(define remberevensXcountevens_cps
  (λ (l k)
    (cond
      [(null? l) (k '(() . 0))]
      [(pair? (car l))
       (remberevensXcountevens_cps (car l)
         (λ (pa)
           (remberevensXcountevens_cps (cdr l)
             (λ (pd)
               (k `(,(cons (car pa) (car pd)) . ,(+ (cdr pa) (cdr pd))))))))]
      [(or (null? (car l)) (odd? (car l)))
       (remberevensXcountevens_cps (cdr l)
         (λ (p)
           (k `(,(cons (car l) (car p)) . ,(cdr p)))))]
      [else (remberevensXcountevens_cps (cdr l)
              (λ (p) (k `(,(car p) . ,(add1 (cdr p))))))])))

(define remberevens
  (λ (l)
    (cond
      [(null? l) (unit_state '())]
      [(pair? (car l))
       (bind_state
         (remberevens (car l))
         (λ (a)
           (bind_state
             (remberevens (cdr l))
             (λ (d) (unit_state (cons a d))))))]
      [(or (null? (car l)) (odd? (car l)))
       (bind_state
         (remberevens (cdr l))
         (λ (d) (unit_state (cons (car l) d))))]
      [else
       (remberevens (cdr l))])))

(define-syntax do*_state
  (syntax-rules ()
    ((_ () body) body)
    ((_ ((a0 ma0) (a ma) ...) body)
     (bind_state
       ma0
       (λ (a0) (do*_state ((a ma) ...) body))))))

;; (do*_state ((a (remberevens_direct (car l)))
;;             (d (remberevens_direct (cdr l))))
;;   (unit_state (cons a d)))

(define remberevensXcountevens_almost
  (λ (l)
    (cond
      [(null? l) (unit_state '())]
      [(pair? (car l))
       (bind_state
         (remberevensXcountevens_almost (car l))
         (λ (a)
           (bind_state
             (remberevensXcountevens_almost (cdr l))
             (λ (d) (unit_state (cons a d))))))]
      [(or (null? (car l)) (odd? (car l)))
       (bind_state
         (remberevensXcountevens_almost (cdr l))
         (λ (d) (unit_state (cons (car l) d))))]
      [else
       (remberevens (cdr l))])))
