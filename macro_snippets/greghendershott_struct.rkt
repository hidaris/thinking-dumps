#lang racket

(require (for-syntax racket/syntax))
(define-syntax (our-struct stx)
  (syntax-case stx ()
    [(_ id (fields ...))
     (for-each (λ (x)
                 (unless (identifier? x)
                   (raise-syntax-error #f "not an identifier" stx x)))
               (cons #'id (syntax->list #'(fields ...))))
     (with-syntax ([pred-id (format-id stx "~a?" #'id)])
       #`(begin
           ; Define a constructor
           (define (id fields ...)
             (apply vector (cons 'id (list fields ...))))
           ; Define a predicate
           (define (pred-id v)
             (and (vector? v)
                  (eq? (vector-ref v 0) 'id)))
           ; Define an accessor for each field.
           #,@(for/list ([x (syntax->list #'(fields ...))]
                         [n (in-naturals 1)])
                (with-syntax ([acc-id (format-id stx "~a-~a" #'id x)]
                              [ix n])
                  #'(define (acc-id v)
                      (unless (pred-id v)
                        (error 'acc-id "~a is not a ~a struct" v 'id))
                      (vector-ref v ix))))))]))

; helper
(define/contract (hash-refs h ks [def #f])
  ((hash? (listof any/c)) (any/c) . ->* . any)
  (with-handlers ([exn:fail? (const (cond [(procedure? def) (def)]
                                          [else def]))])
    (for/fold ([h h])
              ([k (in-list ks)])
      (hash-ref h k))))

(define js (hasheq 'a (hasheq 'b (hasheq 'c "value"))))

(require (for-syntax racket/syntax))
(define-syntax (hash.refs stx)
  (syntax-case stx ()
    ; Check for no args at all
      [(_)
       (raise-syntax-error #f "Expected hash.key0[.key1 ...] [default]"
                           stx #'chain)]
    ; If the optional 'default' is missing, use #f.
    [(_ chain)
     #'(hash.refs chain #f)]
    [(_ chain default)
     (let* ([chain-str (symbol->string (syntax->datum #'chain))]
            [ids (for/list ([str (in-list (regexp-split #rx"\\." chain-str))])
                   (format-id #'chain "~a" str))])
       (with-syntax ([hash-table (car ids)]
                     [keys       (cdr ids)])
         #'(hash-refs hash-table 'keys default)))]))
