#lang eopl
;;; (empty-stack) = []
;;; (push v [v1 v2 v3 ...]) = [v v1 v2 v3 ...]
;;; (pop v [v v1 v2 v3 ...]) = [v1 v2 v3]
;;; (top [v1 v2 v3 ...]) = v
;;; (empty-stack? stack) = #t if stack = [], else #f

;;; constructors : empty-stack push
;;; observers : pop, top, empty-stack?
(define empty-stack
  (lambda ()
    (cons
     (lambda (op)
      (cond
        ((eqv? op 'pop)
         (eopl:error 'pop
                     "stack is empty"))
        ((eqv? op 'top)
         (eopl:error 'top
                     "stack is empty"))
        (else
         (eopl:error `,op
                     "undefined procedure"))))
     (lambda ()
       #t))))

(define push
  (lambda (v saved-stack)
    (cons
     (lambda (op)
      (cond
        ((eqv? op 'pop) saved-stack)
        ((eqv? op 'top) v)
        (else
         (eopl:error `,op
                     "undefined procedure"))))
     (lambda ()
       #f))))

(define pop
  (lambda (stack)
    ((car stack) 'pop)))

(define top
  (lambda (stack)
    ((car stack) 'top)))

(define empty-stack?
  (lambda (stack)
    ((cdr stack))))