#lang racket

;; A type inferencer and normalizer for a small dependently-typed language
;; Adopted from https://github.com/andrejbauer/andromeda

(define lookup-ty
  (lambda (x ctx)
    (cond
      [(assv x ctx) => cadr]
      [else (error "unbound identifier ~s~n" x)])))

(define lookup-value
  (lambda (x ctx)
    (match (assv x ctx)
      [#f (error "unbound identifier ~s~n" x)]
      [`(,x . (,t)) '()]
      [`(,x . (,t ,v)) v])))

(define extend
  (lambda (ctx x t . v)
    `((,x . (,t . ,v)) . ,ctx)))

(define subst
  (let ([subst-abstraction
         (lambda (s abstr)
           (match abstr
             [`(,x ,t ,e)
              (let ((t^ (subst s t)) (x^ (gensym)))
                (let ((e^ (subst `((,x . ,x^) . ,s) e)))
                  `(,x^ ,t^ ,e^)))]))])
    (lambda (s exp)
      (match exp
        [(? number?) exp]
        [(? symbol?) (cond
                       ((assv exp s) => cdr)
                       (else exp))]
        [`(,quant . ,abstr) #:when (memv quant '(pi lambda))
         (let ([abstr (subst-abstraction s abstr)])
           `(,quant . ,abstr))]
        [`(,rator ,rand)
         (let ([rator^ (subst s rator)]
               [rand^ (subst s rand)])
           `(,rator^ ,rand^))]))))

(define normalize
  (let ([normalize-abstraction
         (lambda (ctx abstr)
           (match abstr
             [`(,x ,t ,e)
              (let ([t (normalize ctx t)])
                (let ([e (normalize (extend ctx x t) e)])
                  `(,x ,t ,e)))]))])
    (lambda (ctx exp)
      (match exp
        [(? number?) exp]
        [(? symbol?)
         (let ([v? (lookup-value exp ctx)])
           (if (null? v?) exp (normalize ctx v?)))]
        [`(,quant . ,abstr)
         #:when (memv quant '(pi lambda))
         (let ([abstr (normalize-abstraction ctx abstr)])
           `(,quant . ,abstr))]
        [`(,rator ,rand)
         (let ([rand^ (normalize ctx rand)])
           (match (normalize ctx rator)
             [`(lambda ,x ,_ ,e) (normalize ctx (subst `((,x . ,rand^)) e))]
             [`,neutral `(,neutral ,rand^)]))]))))

(define ctx
  '((N . (0))
    (z . (N))
    (s . ((pi x N N)))
    (three . ((pi y (pi x N N) (pi z N N))
              (lambda f (pi x1 N N)
                      (lambda x N (f (f (f x)))))))))

(pretty-print (normalize ctx '((three (three s)) z)))

(define same?
  (lambda (ctx e1 e2)
    (letrec
        ([same?
          (let ([same-abstraction?
                 (match-lambda**
                  [(`(,x ,t1 ,e1) `(,y ,t2 ,e2))
                   (let ((z (gensym)))
                     (and (same? t1 t2)
                          (same? (subst `((,x . ,z)) e1) (subst `((,y . ,z)) e2))))])])
            (lambda (e1 e2)
              (match* (e1 e2)
                [((? symbol?) (? symbol?)) (eqv? e1 e2)]
                [((? number?) (? number?)) (eqv? e1 e2)]
                [(`(,quant . ,abs1) `(,quant . ,abs2))
                 #:when (memv quant '(pi lambda))
                 (same-abstraction? abs1 abs2)]
                [(`(,rator1 ,rand1) `(,rator2 ,rand2))
                 (and (same? rator1 rator2) (same? rand1 rand2))]
                [(_ _) #f])))])
      (same? (normalize ctx e1) (normalize ctx e2)))))

(define infer-type
  (let ([infer-universe
         (lambda (ctx t)
           (let ([n (normalize ctx (infer-type ctx t))])
             (if (number? n) n (error "type expected ~s~n" n))))]
        (infer-pi
         (lambda (ctx e)
           (match (normalize ctx (infer-type ctx e))
             [`(pi . ,abstr) abstr]
             [else (error "fuction expected ~s~n" pi)]))))
    (lambda (ctx exp)
      (match exp
        [(? symbol?) (lookup-ty exp ctx)]
        [(? number?) (add1 exp)]
        [`(pi ,x ,t1 ,t2)
         (max (infer-universe ctx t1)
              (infer-universe (extend ctx x t1) t2))]
        [`(lambda ,x ,t ,e)
         (begin
           (infer-universe ctx t)
           (let ([te (infer-type (extend ctx x t) e)])
             `(pi ,x ,t ,te)))]
        [`(,rator ,rand)
         (match (infer-pi ctx rator)
           [`(,x ,s ,t)
            (let ([te (infer-type ctx rand)])
              (if (same? ctx s te)
                  (subst `((,x . ,rand)) t)
                  (error "expressions ~s and ~s are not the same~n" s te)))])]))))

(pretty-print (infer-type ctx 'z))
(pretty-print (infer-type ctx 's))
(pretty-print (infer-type ctx '(three s)))
(pretty-print (infer-type ctx '(three (three s))))
