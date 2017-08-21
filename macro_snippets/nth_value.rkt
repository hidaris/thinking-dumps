#lang racket
(require (for-syntax syntax/parse
                     racket/syntax
                     racket/string))

(define-syntax (foo stx)
  (syntax "I am foo"))

(define-syntax (quoted-foo stx)
  #'"I am quoted foo")

(define-syntax (say-hi stx)
  #'(displayln "hi"))

(define-syntax (show-me stx)
  (print stx)
  #'(void))

;; (define stx #'(if x (list "true") #f))

(define-syntax (reverse-me stx)
  (datum->syntax stx (reverse (cdr (syntax->datum stx)))))
                               ;; remove 'reverse-me

(define-syntax (another-foo stx)
  (make-pipe)
  #'(void))

(define (our-if condition true-expr false-expr)
  (cond
    [condition true-expr]
    [else false-expr]))

(define (display-and-return x)
  (displayln x)
  x)

;; due to call-by-value, our-if can't work as primitive if
;; we can fix this by thunk

(define (cbn-if condition true-expr false-expr)
  (cond
    [condition (true-expr)]
    [else (false-expr)]))

;; also, we can use macro

(define-syntax (our-if-v2 stx)
  (define xs (syntax->list stx))
  (datum->syntax stx `(cond [,(cadr xs) ,(caddr xs)]
                            [else ,(cadddr xs)])))

(define stx #'(our-if-v2 #t "true" "false"))
(define xs (syntax->list stx))

(require (for-syntax racket/match))
(define-syntax (our-if-match stx)
  (match (syntax->list stx)
    [`(,name ,condition ,true-expr ,false-expr)
     (datum->syntax stx `(cond [,condition ,true-expr]
                               [else ,false-expr]))]))

(define-syntax (our-if-syntax-case stx)
  (syntax-case stx ()
    [(_ condition true-expr false-expr)
     #'(cond [condition true-expr]
             [else false-expr])]))

(define-syntax (hyphen-define/wrong1 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (let ([name (string->symbol (format "~a-~a" #'a #'b))])
       #'(define (name args ...)
           body0 body ...))]))

(define-syntax (hyphen-define/wrong1.1 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (syntax-case (datum->syntax stx
                                 (string->symbol (format "~a-~a"
                                                         #'a
                                                         #'b)))
         ()
       [name #'(define (name args ...)
                 body0 body ...)])]))

(define-syntax (hyphen-define/ok1 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (syntax-case (datum->syntax stx
                                 (string->symbol (format "~a-~a"
                                                         (syntax->datum #'a)
                                                         (syntax->datum #'b))))
         ()
       [name #'(define (name args ...)
                 body0 body ...)])]))

(define-syntax (hyphen-define/ok2 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (with-syntax ([name  (datum->syntax stx
                              (string->symbol (format "~a-~a"
                                                      (syntax->datum #'a)
                                                      (syntax->datum #'b))))])
       #'(define (name args ...)
           body0 body ...))]))

(define-syntax (hyphen-define/ok3 stx)
  (syntax-case stx ()
    [(_ a b (args ...) body0 body ...)
     (with-syntax ([name  (format-id stx "~a-~a" #'a #'b)])
       #'(define (name args ...)
           body0 body ...))]))

(define-syntax (hyphen-define* stx)
  (syntax-case stx ()
    [(_ (names ...) (args ...) body0 body ...)
     (let* (;; [names/sym (map syntax-e (syntax->list #'(names ...)))]
            [names/sym (syntax->datum #'(names ...))]
            [names/str (map symbol->string names/sym)]
            [name/str (string-join names/str "-")]
            [name/sym (string->symbol name/str)])
       (with-syntax ([name  (datum->syntax stx name/sym)])
         #'(define (name args ...)
             body0 body ...)))]))

(define-syntax (foo2 stx)
  (syntax-case stx ()
    [(_ a)
     (with-syntax* ([b #'a]
                    [c #'b])
       #'c)]))

(define-syntax nth-value
  (syntax-rules ()
    ((_ n values-producing-form)
     (begin
       (display '("Debugging template for nth-value"
                  "n is" n
                  "values-producing-form is" values-producing-form))
       (call-with-values
        (λ () values-producing-form)
        (λ (all-values)
          (list-ref all-values n)))))))

;; (syntax->datum
;;   (expand '(nth-value 1 (let ([a 2] [b 3]) (quotient/remainder a b)))))

(define-syntax test-pattern
  (syntax-rules ()
    ((test-pattern one two) "match 1")
    ((test-pattern one two three) "match 2")
    ((test-pattern . default) "fail")))

(define-syntax my-when
  (syntax-rules ()
    ((_ condition form . forms)
     (if condition
         (begin form . forms)
         #f))))

(define-syntax please
  (syntax-rules ()
    ((please function . args) (function . args))))

(define-syntax syntax-error
  (syntax-rules ()
    [(syntax-error) (syntax-error "Bad use of syntax error!")]))

(define-syntax prohibit-one-arg
  (syntax-rules ()
    [(prohibit-one-arg function argument)
     '(syntax-error
       "Prohibit-one-arg cannot be used with one argument."
       funciton argument)]
    [(prohibit-one-arg function . arguments)
     (function . arguments)]))

(define-syntax my-named-let
  (syntax-rules ()
    [(my-named-let () . ignore)
     (syntax-error "NAME must not be the empty list.")]
    [(my-named-let (car . cdr) . ignore)
     (syntax-error "NAME must be a symbol." (car . cdr))]
    [(my-named-let name bindings form . forms) ;; implicit begin
     (let name bindings form . forms)]))

(define-syntax if*
  (syntax-rules ()
    [(if*) (void)]
    [(if* X) X]
    [(if* C X more ...) (if C X (if* more ...))]))

(define-syntax defmac
  (syntax-rules ()
    [(defmac (name x ...) body)
     (define-syntax name
       (syntax-rules () ((name x ...) body)))]))

;; (define-syntax (while stx)
;;   (define subs (syntax->list stx))
;;   (datum->syntax
;;    stx
;;    `(let loop ()
;;       (when ,(cadr subs)
;;         ,@(cddr subs)
;;         (loop)))
;;    stx))

;; (define-syntax (while stx)
;;   (define subs (syntax->list stx))
;;   (datum->syntax
;;    stx
;;    `(,(quote-syntax let) ,(quote-syntax loop) ()
;;       (,(quote-syntax when) ,(cadr subs)
;;         ,@(cddr subs)
;;         (,(quote-syntax loop))))
;;    stx))

;; (define-syntax (while stx)
;;   (define subs (syntax->list stx))
;;   (datum->syntax
;;    (quote-syntax here)
;;    `(let loop ()
;;       (when ,(cadr subs)
;;         ,@(cddr subs)
;;         (loop)))
;;    stx))

(define-syntax (while stx)
  (syntax-case stx ()
    [(_ test body ...)
     (with-syntax ([it (datum->syntax stx 'it)])
       #'(let loop ()
           (let ([it test])
             (when it
               body ...
               (loop)))))]))

(define x 2)
(let ([let 5])
  (while (< x 10)
    (printf "x = ~s\n" x)
    (set! x (add1 x))))

(define-syntax mlet
  (syntax-rules ()
    [(mlet ([var rhs] ...) body)
     ((λ (var ...) body) rhs ...)]))

;; (define-syntax (nlet stx)
;;   (syntax-case stx ()
;;     [(nlet ([var rhs] ...) body)
;;      (begin
;;        ;; Error-checking code
;;        (for-each (λ (var)
;;                    (unless (identifier? var)
;;                      (wrong-syntax stx 'expected-identifier var)))
;;                  (syntax->list #'(var ...)))
;;        (let ([dup (check-duplicate-identifier (syntax->list #'(var ...)))])
;;          (when dup
;;            (wrong-syntax stx 'duplicate-variable-name dup)))
;;        ;; Transformation expression
;;        #'((λ (var ...) body) rhs ...))]))

(define-syntax (nlet stx)
  (syntax-parse stx
    [(nlet ([var:identifier rhs:expr] ...) body:expr)
     #:fail-when (check-duplicate-identifier (syntax->list #'(var ...)))
                  "duplicate var name"
     #'((λ (var ...) body) rhs ...)]))

;; (define-syntax delay
;;   (syntax-rules ()
;;     [(delay e)
;;      (λ () e)]))
(define-syntax forever
  (syntax-rules ()
    [(_ e)
     (let loop ()
       (begin e (loop)))]))
