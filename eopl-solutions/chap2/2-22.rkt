#lang eopl

;;; (empty-stack) = []
;;; (push v [v1 v2 v3 ...]) = [v v1 v2 v3 ...]
;;; (pop v [v v1 v2 v3 ...]) = [v1 v2 v3]
;;; (top [v1 v2 v3 ...]) = v
;;; (empty-stack? stack) = #t if stack = [], else #f

;;; constructors : empty-stack push
;;; observers : pop, top, empty-stack?

(define-datatype stack stack?
  (empty-stack)
  (push
   (v val?)
   (saved-stack stack?)))

(define val?
  (lambda (v)
    #t))

(define pop
  (lambda (s)
    (cases stack s
           (empty-stack ()
                        (report-no-val-in-the-stack s))
           (push (v saved-stack)
                 saved-stack)
           (else
            (report-invalid-stack s)))))

(define top
  (lambda (s)
    (cases stack s
           (empty-stack ()
                        (report-no-val-in-the-stack s))
           (push (v saved-stack)
                 v)
           (else
            (report-invalid-stack s)))))

(define empty-stack?
  (lambda (s)
    (cases stack s
           (empty-stack ()
                        #t)
           (push (v saved-stack)
                 #f)
           (else
            (report-invalid-stack s)))))

(define report-no-val-in-the-stack
  (lambda (stack)
    (eopl:error 'op
                "can not apply op on a val in ~s." stack)))

(define report-invalid-stack
  (lambda (stack)
    (eopl:error 'op
                "Bad stack ~s." stack)))
