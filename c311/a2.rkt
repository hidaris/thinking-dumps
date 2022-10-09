#lang racket

(define list-ref
  (lambda (ls n)
    (letrec
        ([nth-cdr (lambda (n)
                    (cond
                      [(= n 0) ls]
                      [else (cdr (nth-cdr (- n 1)))]))])
      (car (nth-cdr n)))))

;(list-ref '(a b c) 2)

(define union
  (lambda (l1 l2)
    (cond
      [(null? l1) l2]
      [(memv (car l1) l2) (union (cdr l1) l2)]
      [else (cons (car l1) (union (cdr l1) l2))])))

;(union '(x y) '(x z))


(define walk-symbol
  (lambda (x s)
    (let ([r (assv x s)])
      (cond
        [(not r) x]
        [else (walk-symbol (cdr r) s)]))))


(define lambda-exp?
  (λ (E)
    (letrec
        ([p
          (λ (e)
            (match e
              [(? symbol? y) #t]
              [`(lambda (,(? symbol? x)) ,body) (p body)]
              [`(,rator ,rand) (and (p rator) (p rand))]
              [else #f]))])
      (p E))))


(define var-occurs?
  (λ (x E)
    (letrec
        ([p
          (λ (e)
            (match e
              [(? symbol? y) (eqv? x y)]
              [`(lambda (,(? symbol? x)) ,body) (p body)]
              [`(,rator ,rand) (or (p rator) (p rand))]
              [else #f]))])
      (p E))))


(define vars
  (λ (E)
    (letrec
        ([p
          (λ (e)
            (match e
              [(? symbol? y) `(,y)]
              [`(lambda (,(? symbol? x)) ,body) (p body)]
              [`(,rator ,rand) (append (p rator) (p rand))]
              [else #f]))])
      (p E))))


(define unique-vars
  (λ (E)
    (letrec
        ([p
          (λ (e)
            (match e
              [(? symbol? y) `(,y)]
              [`(lambda (,(? symbol? x)) ,body) (p body)]
              [`(,rator ,rand) (union (p rator) (p rand))]
              [else #f]))])
      (p E))))


(define var-occurs-free?
  (λ (v E)
    (letrec
        ([p
          (λ (e)
            (match e
              [(? symbol? y) (eqv? v y)]
              [`(lambda (,(? symbol? x)) ,body) (and (not (eqv? x v)) (p body))]
              [`(,rator ,rand) (or (p rator) (p rand))]
              [else #f]))])
      (p E))))


(define var-occurs-bound?
  (λ (v E)
    (letrec
        ([p
          (λ (e)
            (match e
              [(? symbol? y) #f]
              [`(lambda (,(? symbol? x)) ,body) (or (and (eqv? x v) (var-occurs? v body)) (p body))]
              [`(,rator ,rand) (or (p rator) (p rand))]
              [else #f]))])
      (p E))))


(define unique-free-vars
  (λ (E)
    (letrec
        ([p
          (λ (e)
            (match e
              [(? symbol? y) `(,y)]
              [`(lambda (,(? symbol? x)) ,body) (p body)]
              [`(,rator ,rand) (union (p rator) (p rand))]
              [else #f]))]
         [q
          (lambda (unique-vars)
            (let ([first-unique-var (car unique-vars)])
              (cond
                [(null? unique-vars) '()]
                [(var-occurs-free? first-unique-var E) (cons first-unique-var (q (cdr unique-vars)))]
                [else (q (cdr unique-vars))])))])
      (q (p E)))))


(define unique-bound-vars
  (λ (E)
    (letrec
        ([p
          (λ (e)
            (match e
              [(? symbol? y) `(,y)]
              [`(lambda (,(? symbol? x)) ,body) (p body)]
              [`(,rator ,rand) (union (p rator) (p rand))]
              [else #f]))]
         [q
          (lambda (unique-vars)
            (let ([first-unique-var (car unique-vars)])
              (cond
                [(null? unique-vars) '()]
                [(var-occurs-bound? first-unique-var E) (cons first-unique-var (q (cdr unique-vars)))]
                [else (q (cdr unique-vars))])))])
      (q (p E)))))


(define fact
  (λ (n result)
    (cond
      [(zero? n) result]
      [else (fact (sub1 n) (* n result))])))


(define a-list `((c . ,(box 15)) (e . ,(box 'f)) (b . ,(box 'c)) (a . ,(box 'b))))


(define walk-symbol-update
  (lambda (x s)
    (let ([r (assv x s)])
      (cond
        [(not r) x]
        [else
         (let ([assc-box (cdr r)])
           (let ([rest-v (walk-symbol-update (unbox assc-box) s)])
             (set-box! assc-box rest-v)
             rest-v))]))))


(define var-occurs-both?
  (λ (v E)
    (letrec
        ([p
          (λ (e)
            (match e
              [(? symbol? y) `(,(eqv? v y) . #f)]
              [`(lambda (,(? symbol? x)) ,body)
               (match-let ([`(,f . ,b) (p body)])
                 (let ([free? (and (not (eqv? x v)) f)]
                       [bound? (or (and (eqv? x v) (var-occurs? v body)) b)])
                   `(,free? . ,bound?)))]
              [`(,rator ,rand)
               (match-let ([`(,f1 . ,b1) (p rator)]
                           [`(,f2 . ,b2) (p rand)])
                 (let ([free? (or f1 f2)]
                       [bound? (or b1 b2)])
                   `(,free? . ,bound?)))]
              [else `(#f . #f)]))])
      (p E))))
