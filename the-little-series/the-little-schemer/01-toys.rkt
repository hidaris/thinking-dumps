#lang racket/base

;; atom? 'a -> bool
(define atom?
  (lambda (a)
    (and (not (null? a)) (not (pair? a)))))
(atom? 'atom)                           ;#t
(atom? 'turkey)                         ;#t
(atom? '1492)                           ;#t
(atom? 'u)                              ;#t
(atom? '*abc$)                          ;#t
(list? '(atom))                         ;#t
(list? '(atom turkey or))               ;#t
;; (list? '(atom turkey) 'or)              ;error list? expect 1 param
(list? '((atom turkey) or))             ;#t
(list? '())                             ;#t
(atom? '())                             ;#f it's a list contains zero S-expressions
(list? '(() () () ()))                  ;#t
(car '(a b c))                          ;a
(car '((a b c) x y z))                  ;(a b c)
(atom? (car '((a b c) x y z)))          ;#f it's a S-expression of list
;;(car 'hotdog)                         ;error car expect a param pair
;;(pair? '())                           ;error null list is not a pair
;;(car '())                             ;error car expect a param pair
;; The Law of Car: The primitive car is defined only for non-empty lists.
(car (cons 1 2))                        ;1
(car '(((hotdogs)) (and) (pickle) relish)) ;((hotdogs))
(car (car '(((hotdogs)) (and))))           ;(hotdogs)
(cdr '(a b c))                             ;(b c)
(cdr '((a b c) x y z))                     ;(x y z)
(cdr '(hamburger))                         ;()
(cdr '((x) t r))                           ;(t r)
;; (cdr 'hotdogs)                          ;error cdr require a pair
;; (cdr '())                               ;error cannot ask for the cdr of null list
;; The primitive cdr is defined only for non-empty lists.
;; The cdr of any non-empty list is always another list.
;; cdr accept a pair and return a list without first S-expression
(car (cdr '((b) (x y) ((c)))))          ;(x y)
(cdr (cdr '((b) (x y) ((c)))))          ;(((c)))
;; (cdr (car '(a (b (c)) d)))           ;error (car l) is an atom, cdr require pair
;; car take any non-empty list as an argument
;; cdr take any non-empty-list as an argument
(cons 'peanut '(butter and jelly))           ;(peanuy butter and jelly)
;; cons adds an atom to the front of a list and return a new list
(cons '(banana and) '(peanut butter and jelly)) ;((banana and) peanut butter and jelly)
(cons '((help) this) '(is very ((hard) to learn))) ;(((help) this) is very ((hard) to learn))
;; What does cons take as its arguments?
;; cons takes two arguments:
;;   the first one is any S-expression;
;;   the second one is any list.
(cons '(a b (c)) '())                   ;((a b (c)))
(cons 'a '())                           ;(a)
(cons '((a b c)) 'b)                    ;(((a b c)) . b)
(cons 'a 'b)                            ;(a . b)
;; The Law of Cons
;; The primitive cons takes two arguments.
;; The second argument to cons must be a list.
;; The result is a list.
;; But two argument actually can be every thing.
;; we use list here because we are cons a list.
(cons 'a (car '((b) c d)))              ;(a b)
(cons 'a (cdr '((b) c d)))              ;(a c d)
(null? '())                             ;#t
(null? (quote ()))                      ;#t (quote ()) == '()
(null? '(a b c))                        ;#f '(a b c) is a non-empty list.
(null? 'spaghetti)                      ;#f we cannot ask null? of an atom. In practice, (null? a) is false for everything, except the empty list.
;; The Law of Null?
;; The primitive null? is defined only for lists.
(atom? 'Harry)                          ;#t Harry is a string of characters beginning with a letter.
(atom? '(Harry had a heap of apples))   ;#f it's a list.
;; How many arguments does atom? take and what are they?
;; It takes one argument. The argument can be any S-expression.
(atom? (car '(Harry had a heap of apples))) ;#t (car l) is 'Harry
(atom? (cdr '(Harry had a heap of apples))) ;#f
(atom? (cdr '(Harry)))                      ;#f '() is not an atom.
(atom? (car (cdr '(swing low sweet cherry oat)))) ;#t 'low is an atom.
(atom? (car (cdr '(swing (low sweet) cherry oat)))) ;#f (low sweet) is a list.
(eq? 'Harry 'Harry)                                 ;#t they are both non-atom Harry
(eq? 'margarine 'butter)                            ;#f they are different atoms.
;; How many arguments does eq? take and what are they?
;; It takes two arguments. Both of them must be non-numeric atoms.
(eq? '() '(strawberry))                 ;#f
(eq? 6 7)                               ;#f
;; The Law of Eq?
;; The primitive eq? takes two arguments. Each must be a non-numeric atom.
(eq? (car '(Mary had a little lamb chop)) 'Mary) ;#t
(eq? (cdr '(soured milk)) 'milk)                 ;#f cdr return a list
(eq? (car '(beans beans we need jelly beans)) (car (cdr '(beans beans we need jelly beans)))) ;#t
