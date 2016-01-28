#lang racket

;; identity monad.
(define return-id
  (λ (a) a))
(define bind-id
  (λ (ma f) (f ma)))

(define plus-id
  (λ (a b)
    (bind-id
     (return-id (+ a b))
     (λ (x) (return-id x)))))

;; maybe monad.
(define return-maybe
  (λ (a)
    `(Just ,a)))

(define bind-maybe
  (λ (ma f)
    (cond
      [(eq? (car ma) 'Just) (f (cadr ma))]
      [(eq? (car ma) 'Nothing) '(Nothing)])))

(define fail
  (λ ()
    '(Nothing)))

(define divide-maybe
  (λ (a b)
    (if (zero? b)
        (fail)
        (return-maybe (/ a b)))))

(bind-maybe
 (return-maybe (+ 7 8))
 (λ (x)
   (bind-maybe
    (divide-maybe x 4)
    (λ (x^)
      (return-maybe x^)))))

(define-syntax dom
  (syntax-rules (<-)
    ((_ bind e) e)
    ((_ bind (v <- e0) e e* ...)
     (bind e0 (λ (v) (dom bind e e* ...))))
    ((_ bind e0 e e* ...)
     (bind e0 (λ (_) (dom bind e e* ...))))))

(dom bind-maybe
  (x <- (return-maybe (+ 7 8)))
  (x^ <- (divide-maybe x 4))
  (return-maybe x^))

;; writer monad.
(define return-writer
  (λ (a)
    `(,a . ())))

(define bind-writer
  (λ (ma f)
    (let ([mb (f (car ma))])
      `(,(car mb) .
        ,(append (cdr ma) (cdr mb))))))

(define tell-writer
  (λ (to-writer)
    `(_ . (,to-writer))))

(define reciprocals
  (λ (l)
    (cond
      [(null? l) (return-writer '())]
      [(zero? (car l))
       (bind-writer
        (tell-writer "Saw a 0")
        (λ (_)
          (reciprocals (cdr l))))]
      [else
       (bind-writer
        (reciprocals (cdr l))
        (λ (d)
          (return-writer
           (cons (/ 1 (car l)) d))))])))

(define reciprocals2
  (λ (l)
    (cond
      [(null? l) (return-writer '())]
      [(zero? (car l))
       (dom bind-writer
         (tell-writer "Saw a 0")
         (reciprocals2 (cdr l)))]
      [else
       (dom bind-writer
         (d <- (reciprocals2 (cdr l)))
         (return-writer
          (cons (/ 1 (car l)) d)))])))

(define return-state
  (λ (a)
    (λ (s)
      `(,a . ,s))))

(define bind-state
  (λ (ma f)
    (λ (s)
      (let ([vs (ma s)])
        (let ([v (car vs)]
              [s^ (cdr vs)])
          ((f v) s^))))))

(define get-state
  (λ (s)
    `(,s . ,s)))

(define put-state
  (λ (new-s)
    (λ (s)
      `(_ . ,new-s))))

(define even-length?
  (λ (l s)
    (cond
      [(null? l) s]
      [else
       (even-length? (cdr l) (not s))])))

(define even-length2?
  (λ (l)
    (cond
      [(null? l) (return-state '_)]
      [else
       (dom bind-state
         (s <- get-state)
         (put-state (not s))
         (even-length2? (cdr l)))])))
