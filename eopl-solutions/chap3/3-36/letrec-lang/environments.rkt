(module environments (lib "eopl.ss" "eopl")

  ;; builds environment interface, using data structures defined in
  ;; data-structures.scm.

  (require "data-structures.rkt")

  (provide init-env empty-env extend-env extend-env-rec apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

  ;; init-env : () -> Env
  ;; usage: (init-env) = [i=1, v=5, x=10]
  ;; (init-env) builds an environment in which i is bound to the
  ;; expressed value 1, v is bound to the expressed value 5, and x is
  ;; bound to the expressed value 10.
  ;; Page: 69

  (define init-env
    (lambda ()
      (extend-env
          'i (num-val 1)
          (extend-env
              'v (num-val 5)
              (extend-env
                  'x (num-val 10)
                  (empty-env))))))

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

  (define empty-env empty-env-record)
  (define extend-env extended-env-record)
  (define extend-env-rec extended-rec-env-record)

  (define empty-env? empty-env-record?)
  (define extend-env? extended-env-record?)

  ;; Page: 86

  (define apply-env
    (lambda (env search-sym)
      (cond
        [(empty-env? env)
         (eopl:error 'apply-env "No binding for ~s" search-sym)]
        [(extend-env? env)
         (let ([sym (extended-env-record->sym env)]
               [val (extended-env-record->val env)]
               [old-env (extended-env-record->old-env env)])
           (if (vector? val)
               ;; extend-rec-env 'proc-table (vector (list (pname (proc-val))))
               (let* ([proc-table (vector-ref val 0)]
                      ;; has-proc : '(pname (proc-val))
                      [has-proc (assoc search-sym proc-table)])
                 (if has-proc
                     (cadr has-proc)
                     (apply-env old-env search-sym)))
               (if (eqv? search-sym sym)
                   val
                   (apply-env old-env search-sym))))])))
  )
