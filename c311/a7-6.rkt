#lang racket

(define cps
  (lambda (e)
    (letrec ([simpl? (lambda (e) (memq e '(zero? add1 sub1)))]
             [end-cont (lambda (v) v)]
             [cont0 (lambda (v) `(k ,v))] ;; `(lambda (x) x) `(if t a b) => (k x) (k a) (k b)
             [fv (let ([n -1])
                   (lambda ()
                     (set! n (+ 1 n))
                     (string->symbol (string-append "v" (number->string n)))))]
             [cps1 (lambda (e cont)
                     (match e
                       [x #:when (not (pair? x)) (cont x)]
                       [`(if ,test ,conseq ,alt)
                        (cps1 test (lambda (t)
                                     (cond
                                       [(memq cont (list cont0 end-cont)) ;; 没有嵌套
                                        `(if ,t
                                             ,(cps1 conseq cont)
                                             ,(cps1 alt cont))]
                                       [else ;; (if (if ...) ...)
                                        (let ([u (fv)])
                                          `(let ([k (lambda (,u) ,(cont u))])
                                             (if ,t ,(cps1 conseq cont0) ,(cps1 alt cont0))))])))]
                       [`(lambda (,x) ,body)
                        (cont `(lambda (,x k)
                                 ,(cps1 body ;; 1. v => (k v) 2. (g c) => (g c k)
                                        cont0   ;; (lambda (v) `(k ,v))
                                        )))]
                       [`(,op ,a ,b)
                        (cps1 a (lambda (v1)
                                  (cps1 b (lambda (v2)
                                            (cont `(,op ,v1 ,v2))))))]
                       [`(,rator ,rand)
                        (cps1 rator
                              (lambda (r)
                                (cps1 rand
                                      (lambda (d)
                                        (cond
                                          [(simpl? r) (cont `(,r ,d))]
                                          [(eq? cont cont0) ; tail call
                                           ; '(lambda (c) (g c))
                                           ; => '(lambda (c k) ,(cps ,(g c) k0))
                                           ; => '(lambda (c k) (g c k))
                                           `(,r ,d k)]
                                          [else
                                           (let ([v* (fv)])
                                             `(,r ,d (lambda (,v*) ,(cont v*))))])))))]))])
      (cps1 e end-cont))))


;; yinwang's test
;; var
(cps 'x)
(cps '(lambda (x) x))
(cps '(lambda (x) (x 1)))


;; no lambda (will generate identity functions to return to the toplevel)
(cps '(if (f x) a b))
(cps '(if x (f a) b))


;; if stand-alone (tail)
(cps '(lambda (x) (if (f x) a b)))


;; if inside if-test (non-tail)
(cps '(lambda (x) (if (if x (f a) b) c d)))


;; both branches are trivial, should do some more optimizations
(cps '(lambda (x) (if (if x (zero? a) b) c d)))


;; if inside if-branch (tail)
(cps '(lambda (x) (if t (if x (f a) b) c)))


;; if inside if-branch, but again inside another if-test (non-tail)
(cps '(lambda (x) (if (if t (if x (f a) b) c) e w)))


;; if as operand (non-tail)
(cps '(lambda (x) (h (if x (f a) b))))


;; if as operator (non-tail)
(cps '(lambda (x) ((if x (f g) h) c)))


;; why we need more than two names
(cps '(((f a) (g b)) ((f c) (g d))))



;; factorial
(define fact-cps
  (cps
   '(lambda (n)
      ((lambda (fact)
         ((fact fact) n))
       (lambda (fact)
         (lambda (n)
           (if (zero? n)
               1
               (* n ((fact fact) (sub1 n))))))))))
