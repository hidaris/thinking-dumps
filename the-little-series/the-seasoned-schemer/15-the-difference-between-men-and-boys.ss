#lang racket/base

(define x
  (cons (quote chicago)
        (cons (quote pizza)
              (quote ()))))

(define gourmet
  (lambda (food)
    (cons food
          (cons x (quote ())))))

(define gourmand
  (lambda (food)
    (set! x food)
    (cons food
          (cons x
                (quote ())))))

(define diner
  (lambda (food)
    (cons (quote milkshake)
          (cons food
                (quote ())))))

(define dinerR
  (lambda (food)
    (set! x food)
    (cons (quote milkshake)
          (cons food
                (quote ())))))

(define omnivore
  (let ((x (quote minestrone)))
    (lambda (food)
      (set! x food)
      (cons food
            (cons x
                  (quote ()))))))
;;; now, we avoid the conflict in dinerR and gourmand
;;; The Sixteenth commandment
;;; Use (set! ...) only with names defined in (let ...)s.

(define gobbler
  (let ((x (quote minestrone)))
    (lambda (food)
      (set! x food)
      (cons food
            (cons x
                  (quote ()))))))

;;; maybe need review.
(define nibbler
  (lambda (food)
    (let ((x (quote donut)))
      (set! x food)
      (cons food
            (cons x
                  (quote ()))))))
;;; The Seventeenth Commandment
;;; Use (set! x ...) for (let ((x ...)) ...) only if
;;; there is at least one (lambda ... between it
;;; and the (let ((x ...)) ...).

(define food (quote none))
(define glutton
  (lambda (x)
    (set! food x)
    (cons (quote more)
          (cons x
                (cons (quote more)
                      (cons x
                            (quote ())))))))

;;; The Eighteenth Commandment
;;; Use (set! x ...) only when the value that x
;;; refers to is no longer needed.

(define chez-nous-wrong
  (lambda ()
    (set! food x)
    (set! x food)))

;;; use (let ...) to make first food value in a temp var.
(define chez-nous
  (lambda ()
    (let ((a food))
      (set! food x)
      (set! x a))))
