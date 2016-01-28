#lang racket


;; ----- data structures -----
(struct Closure (fun env))


;; ----- main code -----
(define r2i
  (lambda (exp)
    (infer exp env0)))

(define infer
  (lambda (exp env)
    (match exp
      [(? symbol? x)
       (let ([v (lookup x env)])
         (cond
           [(not v)
            (error "undefined variable" x)]
           [else v]))]
      [(? number? x) 'int]
      [(? string? x) 'string]
      [(? boolean? x) 'bool]
      [`(lambda (,x) ,e)
       (Closure exp env)]
      [`(let ([,x ,e1]) ,e2)
       (let ([t1 (infer e1 env)])
         (infer e2 (ext-env x t1 env)))]
      [`(if ,t ,e1 ,e2)
       (let ([tt (infer t env)])
         (cond
           [(eq? tt 'bool)
            (let ([t1 (infer e1 env)]
                  [t2 (infer e2 env)])
              (cond
                [(eq? t1 t2) t1]
                [else
                 (failure 'infer
                          "Return types don't match for branches. \n"
                          "branch1: " t1 "\n"
                          "branch2: " t2 "\n")]))]))]
      [`(,e1 ,e2)
       (let ([t1 (infer e1 env)]
             [t2 (infer e2 env)])
         (match t1
           [(Closure `(lambda (,x) ,e) env-save)
            (infer e (ext-env x t2 env-save))]))]
      [`(,op ,e1 ,e2)
       (let ([t1 (infer e1 env)]
             [t2 (infer e2 env)])
         (cond
           [(memq op '(+ - * /))
            (cond
              [(not (eq? t1 'int))
               (failure 'infer "first operand to operator " op " must be int" "\n"
                        "but got: " t1)]
              [(not (eq? t2 'int))
               (failure 'infer "first operand to operator " op " must be int" "\n"
                        "but got: " t2)]
              [else
               'int])]
           [(memq op '(= < >))
            (cond
              [(not (eq? t1 'int))
               (failure 'infer "first operand to operator " op " must be int" "\n"
                        "but got: " t1)]
              [(not (eq? t2 'int))
               (failure 'infer "first operand to operator " op " must be int" "\n"
                        "but got: " t2)]
              [else
               'bool])]))])))


;; ----- environment -----
(define env0 '())

(define ext-env
  (lambda (x v env)
    (cons `(,x . ,v) env)))

(define lookup
  (lambda (x env)
    (let ([p (assq x env)])
      (cond
        [(not p) #f]
        [else (cdr p)]))))


;; ----- utility -----
(define failure
  (lambda (who . args)
    (display who) (display ": ")
    (for-each display args)
    (display "\n")
    (error "type check failed")))



;; ----- examples -----

;; (r2i '(+ 1 2))
;; ;; => 'int


;; (r2i '(* 2 3))
;; ;; => 'int


;; (r2i '(* 2 (+ 3 4)))
;; ;; => 'int


;; (r2i '(* (+ 1 2) (+ 3 4)))
;; ;; => 'int


;; (r2i '((lambda (x) (* 2 x)) 3))
;; ;; => 'int


;; (r2i
;;  '(let ([x 2])
;;     (let ([f (lambda (y) (* x y))])
;;       (f 3))))
;; ;; => 'int


;; (r2i
;;  '(let ([x 2])
;;     (let ([f (lambda (y) (* x y))])
;;       (let ([x "hi"])
;;         (f 3)))))
;; ;; => 'int


;; (r2i
;;  '(let ([x "hi"])
;;     (let ([f (lambda (y) (* x y))])
;;       (let ([x 4])
;;         (f 3)))))
;; ;; infer: first operand to operator * must be int
;; ;; but got: string
;; ;; type check failed


;; (r2i
;;  '(let ([f (lambda (x) x)])
;;     (f "hi")))
;; ;; => 'string


;; (r2i
;;  '(let ([f (lambda (x) x)])
;;     (+ (f 1) (f "hi"))))
;; ;; infer: first operand to operator + must be int
;; ;; but got: string


;; (r2i
;;  '(let ([f (lambda (x) x)])
;;     (((f f) (f f)) (f 1))))
;; ;; => 'int


;; (r2i
;;  '(if (< 1 2)
;;       42
;;       "hi"))
;; ;; infer: Return types don't match for branches.
;; ;; branch1: int
;; ;; branch2: string


;; (r2i
;;  '(if (< 1 2)
;;       "hi"
;;       (if (< 4 3)
;;           100
;;           20)))
;; ;; infer: Return types don't match for branches.
;; ;; branch1: string
;; ;; branch2: int


;; (r2i
;;  '(let ([f (lambda (x)
;;              (lambda (y)
;;                (if (< x 5)
;;                    (+ y 1)
;;                    (* y 2))))])
;;     ((f 1) 2)))
;; ;; => 'int


;; (r2i
;;  '(let ([f (lambda (x)
;;              (lambda (y)
;;                (if (< x 5)
;;                    (+ y 1)
;;                    (* y 2))))])
;;     ((f 1) "hi")))
;; ;; infer: first operand to operator + must be int
;; ;; but got: string


;; (r2i
;;  '(let ([f (lambda (x)
;;              (lambda (y)
;;                (if (< x 5)
;;                    (+ y 1)
;;                    (> y 2))))])
;;     ((f 1) 2)))
;; ;; infer: Return types don't match for branches.
;; ;; branch1: int
;; ;; branch2: bool
