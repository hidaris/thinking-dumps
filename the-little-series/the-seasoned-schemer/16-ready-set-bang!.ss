#lang racket/base

(define sweet-tooth
  (lambda (food)
    (cons food
          (cons (quote cake)
                (quote ())))))

(define last (quote angelfood))

(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food
          (cons (quote cake)
                (quote ())))))

(define ingredients (quote ()))

(define sweet-toothR
  (lambda (food)
    (set! ingredients
          (cons food ingredients))
    (set! last food)
    (cons food
          (cons (quote cake)
                (quote ())))))

;;; The Nineteenth Commandment
;;; Use (set! ...) to remember valuable things between
;;; two distinct uses of a function.
(define Rs (quote ()))
(define Ns (quote ()))
;; (define deepR
;;   (lambda (n)
;;     (let ((result (deep n)))
;;       (set! Rs (cons (deep n) Rs))
;;       (set! Ns (cons n Ns))
;;       result)))

(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond
                ((null? ns) #f)
                ((= (car ns) n) (car rs))
                (else
                 (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

(define deep
  (lambda (m)
    (cond
      ((zero? m) (quote pizza))
      (else (cons (deepM (sub1 m))
                  (quote ()))))))

;; (define deepM
;;   (lambda (n)
;;     (if (member? n Ns)
;;         (find n Ns Rs)
;;         (let ((result (deep n)))
;;           (set! Rs (cons (deep n) Rs))
;;           (set! Ns (cons n Ns))
;;           result))))

(define member?
  (lambda (n ls)
    (cond
      ((null? ls) #f)
      ((eq? (car ls) n) #t)
      (else (member? n (cdr ls))))))

;;; make deepM obey The Sixteenth Commandment
;; (define deepM
;;   (let ((Rs (quote ()))
;;         (Ns (quote ())))
;;     (lambda (n)
;;       (if (atom? (find n Ns Rs))
;;           (let ((result (deep n)))
;;             (set! Rs (cons result Rs))
;;             (set! Ns (cons n Ns))
;;             result)
;;           (find n Ns Rs)))))

;;; follow The Fifteenth Commandment.
(define deepM
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (deep n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))


(define atom?
  (lambda (a)
    (and (not (null? a)) (not (pair? a)))))

;; (define length
;;   (lambda (l)
;;     (cond
;;       ((null? l) 0)
;;       (else (add1 (length (cdr l)))))))

(define length
  (lambda (l)
    0))

(set! length
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))

;;; rewrite by the commandment.
(define lengthr
  (let ((h (lambda (l) 0)))
    (set! h
          (lambda (l)
            (cond
              ((null? l) 0)
              (else (add1 (h (cdr l)))))))
    h))

;;; It is as if we had written:
(define h1
  (lambda (l)
    0))

(define lengthl
  (let ()
    (set! h1
          (lambda (l)
            (cond
              ((null? l) 0)
              (else (add1 (h1 (cdr l)))))))
    h1))

;;; It is as if we had written:
(define h2
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (h2 (cdr l)))))))

(define lengthll
  (let ()
    h2))

;;; It is as if we had written:
(define lengthh
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (h2 (cdr l)))))))

;;; This means lengthr would perform as we
;;; expect it to.

;;; It would because it is basically the same
;;; function it used to be. It just refers to a
;;; recursive copy of itself through the imaginary
;;; name h1.

;; (define length
;;   (let ((h (lambda (l) 0)))
;;     (set! h ...)
;;     h))
;; The rest could be reused to construct
;; any recursive function of one argument.

(define L
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))

(define another-length
  (let ((h (lambda (l) 0)))
    (set! h
          (L (lambda (arg) (h arg))))
    h))
;;; according to η-reduction, (λ (arg) (h arg)) = h
;;; so, (L (λ (arg) (h arg))) = (L h)

;; what is the value of
;; (L (lambda (arg) (h arg)))
;; it is a function:
;; (lambda (l)
;;   (cond
;;     ((null? l) 0)
;;     (else (add1
;;            ((lambda (arg) (h arg))
;;             (cdr l))))))
;; what is the value of that?
;; we don't know because h changes. Indeed, it changes
;; and becomes this function.
;; and then the value of h is the recursive function length.

(define Y!
  (lambda (L)
    (let ((h (lambda (l) (quote ()))))
      (set! h
            (L (lambda (arg) (h arg))))
      h)))

;;; can we explain Y-bang?
(define Y-bang
  (lambda (f)
    (letrec
        ((h (f (lambda (arg) (h arg)))))
      h)))
;;; here are our words:
;;; "A (letrec ...) is an abbreviation for an expression consisting of
;;; (let ...) and (set! ...). so another way of writing Y! is Y-bang."
;;; A (letrec ...) that defines mutually recursive definitions can be
;;; abbreviated using (let ...) and (set! ...) expressions:
;; (letrec
;;     ((x1 α1)
;;      ...
;;      (xn αn))
;;   β)
;; =
;; (let ((x1 0) ... (xn 0))
;;   (let ((y1 α1) ... (yn αn))
;;     (set! x1 y1)
;;     ...
;;     (set! xn yn))
;;   β)
;;; neew review.

;;; write length with Y!
(define lengthY (Y! L))

;;; define D so that depth* is (define depth* (Y! D)
(define D
  (lambda (depth*)
    (lambda (s)
      (cond
        ((null? s) 1)
        ((atom? (car s))
         (depth* (cdr s)))
        (else
         (max
          (add1 (depth* (car s)))
          (depth* (cdr s))))))))

(define depth* (Y! D))
;;; definition to a function f such that (Y! f)
;;; builds the corresponding recursive funtion
;;; without (define ...)
;;; the name of recursive function is recfun
;;; and the whole expression is wrapped in
;;; (lambda (recfun) ...)

;;; It is true that the value of (Y f) is the same
;;; recursive funtion as the value of (Y! f) for all
;;; f that has this shape.

(define biz
  (let ((x 0))
    (lambda (f)
      (set! x (add1 x))
      (lambda (a)
        (if (= a x)
            0
            (f a))))))
;; ((Y biz) 5) => 0
;; ((Y! biz) 5) => not 0, It doesn't even have an answer.
