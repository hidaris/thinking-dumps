(module store (lib "eopl.ss" "eopl")

  (require "drscheme-init.rkt"
           "data-structures.rkt"
           racket/vector)

  (provide empty-store reference? newref deref setref! store?
           instrument-newref get-store-as-list)

  (define instrument-newref (make-parameter #f))

  ;;;;;;;;;;;;;;;; references and the store ;;;;;;;;;;;;;;;;

  ;;; world's dumbest model of the store:  the store is a list and a
  ;;; reference is number which denotes a position in the list.

  ;; the-store: a Scheme variable containing the current state of the
  ;; store.  Initially set to a dummy variable.
  ;; (define the-store 'uninitialized)

  ;; empty-store : () -> Sto
  ;; Page: 111
  (define empty-store
    (lambda () (make-vector 0)))

  ;; initialize-store! : () -> Sto
  ;; usage: (initialize-store!) sets the-store to the empty-store
  ;; Page 111
  ;; (define initialize-store!
  ;;   (lambda ()
  ;;     (set! the-store (empty-store))))

  ;; get-store : () -> Sto
  ;; Page: 111
  ;; This is obsolete.  Replaced by get-store-as-list below
  ;; (define get-store
  ;;   (lambda () the-store))

  ;; reference? : SchemeVal -> Bool
  ;; Page: 111
  (define reference?
    (lambda (v)
      (integer? v)))

  (define store?
    (lambda (v)
      (vector? v)))

  ;; newref : ExpVal -> Ref
  ;; Page: 111
  (define newref
    (lambda (val store)
      (let ((next-ref (vector-length store)))
        (set! store
              (vector-append store (vector val)))
        (when (instrument-newref)
          (eopl:printf
           "newref: allocating location ~s with initial contents ~s~%"
           next-ref val))
        (an-answer (num-val next-ref) store))))

  ;; deref : Ref -> ExpVal
  ;; Page 111
  (define deref
    (lambda (ref store)
      (vector-ref store ref)))

  ;; setref! : Ref * ExpVal -> Unspecified
  ;; Page: 112
  (define setref!
    (lambda (ref val store)
      (if (< (vector-length store)
             (+ ref 1))
          (report-invalid-reference ref store)
          (vector-set! store ref val))
      store))

  (define report-invalid-reference
    (lambda (ref store)
      (eopl:error 'setref
                  "illegal reference ~s in store ~s"
                  ref store)))

  ;; get-store-as-list : () -> Listof(List(Ref,Expval))
  ;; Exports the current state of the store as a scheme list.
  ;; (get-store-as-list '(foo bar baz)) = ((0 foo)(1 bar) (2 baz))
  ;;   where foo, bar, and baz are expvals.
  ;; If the store were represented in a different way, this would be
  ;; replaced by something cleverer.
  ;; Replaces get-store (p. 111)
  (define get-store-as-list
    (lambda (store)
      (letrec
          ((inner-loop
            ;; convert sto to list as if its car was location n
            (lambda (sto n)
              (if (null? sto)
                  '()
                  (cons
                   (list n (car sto))
                   (inner-loop (cdr sto) (+ n 1)))))))
        (inner-loop (vector->list store) 0))))

  )
