#lang racket

(define last-non-zero
  (lambda (ls)
    (let/cc k
      (letrec
          ([last-non-zero
            (lambda (ls)
              (cond
                [(null? ls) '()]
                [(zero? (car ls)) (k (last-non-zero (cdr ls)))]
                [else (cons (car ls) (last-non-zero (cdr ls)))]))])
        (last-non-zero ls)))))


(define lex
  (λ (e acc)
    (match e
      [`,n #:when (number? n)
       `(const ,n)]
      [`,y #:when (symbol? y)
       `(var ,(index-of acc y))]
      [`(lambda (,x) ,body)
       #:when (symbol? x)
       `(lambda ,(lex body (cons x acc)))]
      [`(zero? ,y)
       `(zero? ,(lex y acc))]
      [`(* ,e1 ,e2)
       `(mult ,(lex e1 acc) ,(lex e2 acc))]
      [`(let/cc ,k ,body)
       `(letcc ,(lex body (cons k acc)))]
      [`(throw ,e1 ,e2)
       `(throw ,(lex e1 acc) ,(lex e2 acc))]
      [`(,rator ,rand)
       `(app ,(lex rator acc) ,(lex rand acc))])))