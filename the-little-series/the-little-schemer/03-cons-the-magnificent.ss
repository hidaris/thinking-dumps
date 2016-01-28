;; rember
;; "Rember" stands for remove a member
;; such as (rember 'mint '(lamb chops and mint jelly)) return (lamb chops and jelly)
;; what does (rember a lat) do?
;; It takes an atom and a lat as its arguments,
;; and makes a new lat with the first occurrence of the atom in the old lat removed.
;; (define rember
;;   (lambda (a lat)
;;     (cond
;;      ((null? lat) (quote ()))
;;      (else (cond
;;             ((eq? (car lat) a) (cdr lat))
;;             (else (cons (car lat)
;;                         (rember a
;;                                 (cdr lat)))))))))
;; we can also simplify it without two else
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat)
                 (rember a
                         (cdr lat)))))))
(rember 'baaa '(aaa baaa ddddd))        ;(aaa ddddd)
(rember 'aaa '())                       ;()

;; (firsts l) where l is ((apple peach) (plum pear) (grape raisin) (bean carrot)) return (apple plum grape bean)
;; (first '()) return ()

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l))
                 (firsts (cdr l)))))))
(firsts '((a b)
          (g c)
          (k n)))
(firsts '())
;; (insertR new old lat) new = topping old = fudge
;; lat = (ice cream with fudge for dessert)
;; return (ice cream with fudge topping for dessert)

(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) old)
             (cons old
                   (cons new (cdr lat))))
            (else (cons (car lat)
                        (insertR new old
                                 (cdr lat)))))))))
(insertR 'topping 'fudge
         '(ice cream with fudge for dessert))
(insertR 'a 'b
         '(d))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) old)
             ;; (cons new (cons old (cdr lat))))
             ;; => (cons old (cdr lat)) == lat
             (cons new lat))
            (else (cons (car lat)
                        (insertL new old
                                 (cdr lat)))))))))
(insertL 'a 'b
         '(a b c))

;; (subst new old lat) => lat with new replace old
(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) old)
             (cons new (cdr lat)))
            (else (cons (car lat)
                        (subst new old
                               (cdr lat)))))))))
(subst 'a 'b
       '(b c))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     (else (cond
            ;; ((eq? (car lat) o1)
            ;;  (cons new (cdr lat)))
            ;; ((eq? (car lat) o2)
            ;;  (cons new (cdr lat)))
            ((or (eq? (car lat) o1) (eq? (car lat) o2))
             (cons new (cdr lat)))
            (else (cons (car lat)
                        (subst2 new o1 o2
                                (cdr lat)))))))))
(subst2 'a 'b 'c
        '(j d c g))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) a)
             (multirember a (cdr lat)))
            (else (cons (car lat)
                        (multirember a
                                     (cdr lat)))))))))
(multirember 'a '(d g a c b a d))

(define multiinsertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) old)
             (cons old
                   ;; old can be replaced with (car lat) here
                   (cons new
                         (multiinsertR new old
                                       (cdr lat)))))
            (else (cons (car lat)
                        (multiinsertR new old
                                      (cdr lat)))))))))
(multiinsertR 'a 'b
              '(c d b f b k))

(define multiinsertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) old)
             (cons new
                   (cons old
                         (multiinsertL new old
                                       (cdr lat)))))
            (else (cons (car lat)
                        (multiinsertL new old
                                      (cdr lat)))))))))
(multiinsertL 'a 'b
              '(c d b f b k))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                 The Fourth Commandment                     ;;
;; Always change at least one argument while recurring. It    ;;
;; must be changed to be closer to termination. The changing  ;;
;; argument must be tested in the termination condition:      ;;
;; when using cdr, test termination with null?                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else (cond
            ((eq? (car lat) old)
             (cons new
                   (multisubst new old
                               (cdr lat))))
            (else (cons (car lat)
                        (multisubst new old
                                    (cdr lat)))))))))
(multisubst 'a 'b
            '(c b d b))
