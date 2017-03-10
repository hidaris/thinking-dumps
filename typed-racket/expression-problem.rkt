#lang typed/racket
(require (for-syntax typed/racket/base racket/list racket/syntax)
         racket/match syntax/parse/define racket/format)

;; from extensible functions package
(define-syntax-parser define/match/extensible
  [(_ (~and form (function:identifier argument:identifier ...)) (~datum :) type:expr
      clause/original:expr ...)
   (with-syntax ([define/match/extension/function
                   (format-id #'function "define/match/extension/~a" #'function)])
     #`(begin
         (: function type)
         (define form
           ((current-function) argument ...))

         (define-simple-macro (define/match/extension/function clause:expr (... ...+))
           (current-function
            (let ([rest-function (current-function)])
              (: function type)
              (define/match form
                clause (... ...)
                [(argument ...) (rest-function argument ...)])
              function)))

         (: current-function (Parameterof type))
         (define current-function
           (make-parameter
            (let ()
              (: function type)
              (define form
                (apply raise-arguments-error 'function
                       "match/extensible function not defined for arguments"
                       (append `(,(~a 'argument) ,argument) ...)))
              function)))

         #,@(if (empty? (syntax->list #'(clause/original ...)))
                #'()
                #'((define/match/extension/function clause/original ...)))))])

(struct Expr ())
(struct Val Expr ([v : Integer]) #:transparent)
(struct Add Expr ([e1 : Expr] [e2 : Expr]) #:transparent)
(struct Mul Expr ([e1 : Expr] [e2 : Expr]) #:transparent)

;; (: eval : Expr -> Integer)
;; (define (eval e)
;;   (match e
;;     [(Val n) n]
;;     [(Add e1 e2) (+ (eval e1)
;;                     (eval e2))]
;;     [(Mul e1 e2) (- (eval e1)
;;                     (eval e2))]))

;; (struct Mul Expr ([e1 : Expr] [e2 : Expr]) #:transparent)

;; (: evalM (-> Expr Integer))
;; (define (evalM e)
;;   (cond
;;     [(Mul? e) (- (eval (Mul-e1 e))
;;                  (eval (Mul-e2 e)))]
;;     [(Expr? e) (eval e)]))
(define/match/extensible (eval e)
  : (-> Expr Integer)
  [((Val n)) n]
  [((Add e1 e2)) (+ (eval e1)
                    (eval e2))])

(define/match/extension/eval
  [((Mul e1 e2)) (- (eval e1)
                    (eval e2))])
