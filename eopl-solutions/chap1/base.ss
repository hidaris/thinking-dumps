#lang eopl

(provide equal??)

(define-syntax equal??
  (syntax-rules ()
    ((_ test-exp correct-ans)
     (let ((observed-ans test-exp))
       (if (not (equal? observed-ans correct-ans))
           (eopl:printf "~s returned ~s, should have returned ~s~%"
                   'test-exp
                   observed-ans
                   correct-ans)
           (begin
             (eopl:printf "#t ~%")))))))
