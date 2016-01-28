#lang racket

(define ack
  (lambda (k)
    (lambda (s t)
      (cond
        [(zero? k) (add1 t)]
        [(zero? (sub1 k))
         (cond
           [(zero? t) s]
           ; (ack (sub1 k)) => succ => add1 t
           ; (add1 (+ s (sub1 t))) => add1 add1 ... add1 s
           [else ((ack (sub1 k)) s ((ack k) s (sub1 t)))])]
        [(zero? (sub1 (sub1 k)))
         (cond
           [(zero? t) 0]
           ; (ack (sub1 k)) => +
           ; (+ s (* s (sub1 t))) => s + s ... + s + 0
           [else ((ack (sub1 k)) s ((ack k) s (sub1 t)))])]
        [(zero? t) 1]
        ; (ack (sub1 k)) => *
        ; (* s (^ s (sub1 t))) => s * s * ... * s * 1
        ; (ack (sub1 k)) => ^
        ; (^ s (^^ s (sub1 t))) => (^ s (^ s (^ s ... (^ s 0))))
        ; => (^ 2 (^ 2 (^ 2 1)))
        [else ((ack (sub1 k)) s ((ack k) s (sub1 t)))]))))

(define succ (ack 0))
(define o+ (ack 1))
(define o* (ack 2))
(define ^ (ack 3))
(define ^^ (ack 4))

(define ack2
  (lambda (k)
    (lambda (s t)
      (cond
        [(zero? t) (cond
                     [(zero? k) 1]
                     [(zero? (sub1 k)) s]
                     [(zero? (sub1 (sub1 k))) 0]
                     [else 1])]
        [(zero? k) (add1 t)]
        [else ((ack2 (sub1 k)) s ((ack2 k) s (sub1 t)))]))))

(define ack3
  (lambda (k)
    (lambda (s t)
      (cond
        [(zero? k) (add1 t)]
        [(zero? t) (cond   ;;; (ack (sub1 k) _ 1)
                     [(zero? (sub1 k)) s]
                     [(zero? (sub1 (sub1 k))) 0]
                     [else 1])]
        [else ((ack3 (sub1 k)) s ((ack3 k) s (sub1 t)))]))))

(define ack4
  (λ (k t)
    (cond
      [(zero? k) (add1 t)]
      [(zero? t) (ack4 (sub1 k) 1)]
      [else (ack4 (sub1 k) (ack4 k (sub1 t)))])))
; (ack4 2 3)
; => (ack4 1 (ack4 2 2))
; => (ack4 1 (ack4 1 (ack4 2 1)))
; => (ack4 1 (ack4 1 (ack4 1 (ack4 2 0))))
; => (ack4 1 (ack4 1 (ack4 1 (ack4 1 1))))
; => (ack4 1 (ack4 1 (ack4 1 (ack4 0 (ack4 1 0)))))
; => (ack4 1 (ack4 1 (ack4 1 (add1 (ack4 1 0)))))
; => (ack4 1 (ack4 1 (ack4 1 (add1 (ack4 0 1)))))
; => (ack4 1 (ack4 1 (ack4 1 (add1 2))))
; => (ack4 1 (ack4 1 (ack4 1 3)))
; => (ack4 1 (ack4 1 (add1 (ack4 1 2)))
; => (ack4 1 (ack4 1 (add1 (ack4 0 (ack4 1 1))))
; => (ack4 1 (ack4 1 (add1 (ack4 0 3)))
; => (ack4 1 (ack4 1 (add1 4))
; => (ack4 1 (ack4 1 5)
; => (ack4 1 (ack4 0 (ack4 1 4))
; => (ack4 1 (ack4 0 (ack4 0 (ack4 1 3))))
; => (ack4 1 (ack4 0 (ack4 0 5)))
; => (ack4 1 (ack4 0 6))
; => (ack4 1 7)
; => (ack4 0 (ack4 1 6))
; => (ack4 0 (ack4 0 (ack4 1 5)))
; => (add1 (add1 7))
; => 9
