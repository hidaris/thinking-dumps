#lang racket

(require racket/fixnum
         "utilities.rkt")

(define ast1.4 '(- 8))
(define ast1.1 `(+ (read) ,ast1.4))

(match ast1.1
  [`(,op ,child1 ,child2)
     (print op) (newline)
     (print child1) (newline)
     (print child2) (newline)])

(define (leaf? arith)
  (match arith
    [(? fixnum?) #t]
    [`(read) #t]
    [`(- ,c1) #f]
    [`(+ ,c1 ,c2) #f]))

(leaf? '(read))
(leaf? '(- 8))
(leaf? '(+ (read) (- 8)))

(define (R0? sexp)
  (match sexp
    [(? fixnum?) #t]
    [`(read) #t]
    [`(- ,e) (R0? e)]
    [`(+ ,e1 ,e2)
     (and (R0? e1) (R0? e2))]
    [`(program ,e) (R0? e)]
    [else #f]))

(R0? `(+ (read) (- 8)))
(R0? `(- (read) (+ 8)))

(define (interp-R0 e)
  (match e
    [(? fixnum?) e]
    [`(read)
       (let ([r (read)])
         (cond [(fixnum? r) r]
               [else (error 'interp-R0 "input␣not␣an␣integer" r)]))]
    [`(- ,(app interp-R0 v))
       (fx- 0 v)]
    [`(+ ,(app interp-R0 v1) ,(app interp-R0 v2))
       (fx+ v1 v2)]
    [`(program ,(app interp-R0 v)) v]
    ))

;; (define (test-pe p)
;;   (assert "testing␣pe-arith"
;;           (equal? (interp-R0 p) (interp-R0 (pe-arith p)))))


(define (pe-neg r)
  (cond
    [(fixnum? r) (fx- 0 r)]
    [else `(- ,r)]))

(define (pe-add r1 r2)
  (cond
    [(and (fixnum? r1) (fixnum? r2)) (fx+ r1 r2)]
    [else `(+ ,r1 ,r2)]))

;; old
;; exp ::= int | (read) | (- exp) | (+ exp exp)
;; new
;; exp ::= (read) | (- (read)) | (+ exp exp)
;; residual ::= int | (+ int exp) | exp

(define (pe-residual e)
  (match e
    [(? fixnum?) e]
    [`(+ ,(? fixnum? r1) ,r2)
     (pe-add r1 r2)]
    [`(+ ,r1 ,(? fixnum? r2))
     (pe-add r1 r2)]
    [else (pe-exp e)]))

(define (pe-exp e)
  (match e
    [`(read) `(read)]
    [`(- ,(app pe-residual r1))
     (pe-neg r1)]
    [`(+ ,(app pe-exp r1) ,(app pe-exp r2))
     `(+ ,r1 ,r2)]))

;; (test-pe '(+ (read) (- (+ 5 3))))
;; (test-pe '(+ 1 (+ (read) 1)))
;; (test-pe '(- (+ (read) (- 5))))
