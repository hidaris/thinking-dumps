#lang typed/racket

(struct (a) Show
  ([show : (→ a String)]))

(: show_bool (Show Boolean))
(define show_bool
  (Show
   (λ (bool)
     (match bool
       [True "True"]
       [False "False"]))))

(: show_int (Show Integer))
(define show_int
  (Show number->string))

(: print (∀ (a)
            (→ (Show a)
                (→ a Void))))
(define (print show)
  (λ (y)
    (let ([show (Show-show show)])
      (println (show y)))))

(: test_print : (→ Void))
(define test_print
  (λ ()
    ((print show_bool) #t)))

(struct (a) Num
  ([fromInt : (→ Integer a)]
   [add : (→ a a a)]))

;; (: sum (∀ (a)
;;           (→ (Num a) (Listof a) a)))
;; (define (sum num ls)
;;   (foldr (Num-add num) ((Num-fromInt num) 0) ls))

(: o+ (→ Integer Integer Integer))
(define (o+ n m) (+ n m))

(: num_int (Num Integer))
(define num_int
  (Num
   (λ ([x : Integer]) x)
   o+))

(: print_incr (∀ (a)
                 (→ (Show a) (Num a)
                     (→ a Void))))
(define (print_incr show num)
  (λ (x)
    (let ([+ (Num-add num)]
          [fromInt (Num-fromInt num)])
      ((print show) (+ x (fromInt 1))))))

(: print_incr_int (→ Integer Void))
(define (print_incr_int x)
  ((print_incr show_int num_int) x))
