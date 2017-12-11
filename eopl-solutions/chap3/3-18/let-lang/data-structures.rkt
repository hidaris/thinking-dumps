(module data-structures (lib "eopl.ss" "eopl")

  ;; data structures for let-lang.

  (provide (all-defined-out))   ; too many things to list

;;;;;;;;;;;;;;;; expressed values ;;;;;;;;;;;;;;;;

;;; an expressed value is either a number, a boolean or a procval.

  (define-datatype expval expval?
    (num-val
     (value number?))
    (bool-val
     (boolean boolean?))
    (cons-val
     (car expval?)
     (cdr expval?))
    (emptylist-val))

;;; extractors:

  ;; expval->num : ExpVal -> Int
  ;; Page: 70
  (define expval->num
    (lambda (v)
      (cases expval v
        (num-val (num) num)
        (else (expval-extractor-error 'num v)))))

  ;; expval->bool : ExpVal -> Bool
  ;; Page: 70
  (define expval->bool
    (lambda (v)
      (cases expval v
        (bool-val (bool) bool)
        (else (expval-extractor-error 'bool v)))))

  (define expval-extractor-error
    (lambda (variant value)
      (eopl:error 'expval-extractors "Looking for a ~s, found ~s"
                  variant value)))

  (define expval->car
    (lambda (v)
      (cases expval v
        (cons-val (car cdr) car)
        (else (expval-extractor-error 'car v)))))

  (define expval->cdr
    (lambda (v)
      (cases expval v
        (cons-val (car cdr) cdr)
        (else (expval-extractor-error 'cdr v)))))

  (define expval->emptylist
    (lambda (v)
      (cases expval v
        (emptylist-val () '())
        (else (expval-extractor-error 'emptylist v)))))

  (define emptylist?
    (lambda (v)
      (cases expval v
        (emptylist-val () #t)
        (else #f))))

  (define cons-val?
    (lambda (v)
      (cases expval v
        (cons-val (car cdr) #t)
        (else #f))))

  ;; add by 3-10, hidaris
  ;; Listof(ExpVal) -> ExpVal, more precisely cons-val
  (define slist->dlist
    (lambda (slist)
      (cond
        [(null? slist) (emptylist-val)]
        [else (cons-val (car slist)
                        (slist->dlist (cdr slist)))])))

  (define print
    (lambda (val)
      (display (expval->exp val))
      (newline)
      (num-val 1)))

  (define expval->exp
    (lambda (val)
      (cases expval val
        (num-val (num)
                 num)
        (bool-val (bool)
                  bool)
        (emptylist-val ()
                       '())
        (cons-val (car cdr)
                  (cons (expval->exp car)
                        (expval->exp cdr))))))

;;;;;;;;;;;;;;;; environment structures ;;;;;;;;;;;;;;;;

  ;; example of a data type built without define-datatype

  (define empty-env-record
    (lambda ()
      '()))

  (define extended-env-record
    (lambda (sym val old-env)
      (cons (list sym val) old-env)))

  (define empty-env-record? null?)

  (define environment?
    (lambda (x)
      (or (empty-env-record? x)
          (and (pair? x)
               (symbol? (car (car x)))
               (expval? (cadr (car x)))
               (environment? (cdr x))))))

  (define extended-env-record->sym
    (lambda (r)
      (car (car r))))

  (define extended-env-record->val
    (lambda (r)
      (cadr (car r))))

  (define extended-env-record->old-env
    (lambda (r)
      (cdr r)))
  )
