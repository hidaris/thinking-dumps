#lang racket

(require "mk.rkt")

(defrel (caro p a)
  (fresh (d)
    (== (cons a d) p)))

(defrel (cdro p d)
  (fresh (a)
    (== (cons a d) p)))

(defrel (conso a d p)
  (== `(,a . ,d) p))

(defrel (pairo p)
  (fresh (a d)
    (conso a d p)))

(defrel (nullo p)
  (== '() p))

(defrel (singletono l)
  (fresh (d)
    ;; (cdro l d)
    ;; (nullo d)
    (== `(,d) l)
    ))

(defrel (listo l)
  (conde
   [(nullo l)]
   [(fresh (d)
      (cdro l d)
      (listo d))]))

(defrel (lolo l)
  (conde
   [(nullo l)]
   [(fresh (a)
      (caro l a)
      (listo a))
    (fresh (d)
      (cdro l d)
      (lolo d))]))


(defrel (loso l)
  (conde
   [(nullo l)]
   [(fresh (a d)
      (conso a d l)
      (singletono a)
      (loso d))]))

(defrel (membero x l)
  (conde
   [(caro l x)]
   [(fresh (d)
      (cdro l d)
      (membero x d))]))

(defrel (proper-membero x l)
  (conde
   [(caro l x)
    (fresh (d)
      (cdro l d)
      (listo d))]
   [(fresh (d)
      (cdro l d)
      (proper-membero x d))]))

(defrel (appendo l t out)
  (conde
   [(nullo l) (== t out)]
   [(fresh (a d res)
      (conso a d l)
      (conso a res out)
      (appendo d t res))]))

(defrel (unwrapo x out)
  (conde
   ((fresh (a)
      (caro x a)
      (unwrapo a out)))
   ((== x out))))

(defrel (memo x l out)
  (conde
   [(caro l x)
    (== l out)]
   [(fresh (d)
      (cdro l d)
      (memo x d out))]))

(defrel (rembero x l out)
  (conde
   [(nullo l) (== out '())]
   [(conso x out l)]
   [(fresh (a d res)
      (conso a d l)
      (conso a res out)
      (rembero x d res))]))

(defrel (alwayso)
  (conde
   [succeed]
   [(alwayso)]))

(defrel (nevero)
  (nevero))

;; (defrel (bit-xoro x y r)
;;   (conde
;;    [(== 0 x) (== 0 y) (== 0 r)]
;;    [(== 0 x) (== 1 y) (== 1 r)]
;;    [(== 1 x) (== 0 y) (== 1 r)]
;;    [(== 1 x) (== 1 y) (== 0 r)]))
(defrel (bit-nando x y r)
  (conde
   [(== 0 x) (== 0 y) (== 1 r)]
   [(== 0 x) (== 1 y) (== 1 r)]
   [(== 1 x) (== 0 y) (== 1 r)]
   [(== 1 x) (== 1 y) (== 0 r)]))

(defrel (bit-xoro x y r)
  (fresh (s t u)
    (bit-nando x y s)
    (bit-nando s y u)
    (bit-nando x s t)
    (bit-nando t u r)))

;; (defrel (bit-ando x y r)
;;   (conde
;;    [(== 0 x) (== 0 y) (== 0 r)]
;;    [(== 1 x) (== 0 y) (== 0 r)]
;;    [(== 0 x) (== 1 y) (== 0 r)]
;;    [(== 1 x) (== 1 y) (== 1 r)]))

(defrel (bit-noto x r)
  (bit-nando x x r))

(defrel (bit-ando x y r)
  (fresh (s)
    (bit-nando x y s)
    (bit-noto s r)))

(defrel (half-addero x y r c)
  (bit-xoro x y r)
  (bit-ando x y c))

(defrel (full-addero b x y r c)
  (fresh (w xy wz)
    (half-addero x y w xy)
    (half-addero w b r wz)
    (bit-xoro xy wz c)))

(define (build-num n)
  (cond
    [(odd? n)
     (cons 1 (build-num (/ (sub1 n) 2)))]
    [(and (not (zero? n)) (even? n))
     (cons 0 (build-num (/ n 2)))]
    [(zero? n) '()]))

(defrel (poso n)
  (fresh (a d)
    (== `(,a . ,d) n)))

(defrel (>1o n)
  (fresh (a ad dd)
    (== `(,a ,ad . ,dd) n)))

(defrel (addero b n m r)
  (conde
    ((== 0 b) (== '() m) (== n r))
    ((== 0 b) (== '() n) (== m r)
     (poso m))
    ((== 1 b) (== '() m)
     (addero 0 n '(1) r))
    ((== 1 b) (== '() n) (poso m)
     (addero 0 '(1) m r))
    ((== '(1) n) (== '(1) m)
     (fresh (a c)
       (== `(,a ,c) r)
       (full-addero b 1 1 a c)))
    ((== '(1) n) (gen-addero b n m r))
    ((== '(1) m) (>1o n) (>1o r)
     (addero b '(1) n r))
    ((>1o n) (gen-addero b n m r))))

(defrel (gen-addero b n m r)
  (fresh (a c d e x y z)
    (== `(,a . ,x) n)
    (== `(,d . ,y) m) (poso y)
    (== `(,c . ,z) r) (poso z)
    (full-addero b a d c e)
    (addero e x y z)))

(defrel (+o n m k)
  (addero 0 n m k))

(defrel (-o n m k)
  (+o m k n))

(defrel (lengtho l n)
  (conde
   [(nullo l) (== '() n)]
   [(fresh (d res)
      (cdro l d)
      (+o '(1) res n)
      (lengtho d res))]))

(defrel (*o n m p)
  (conde
   [(== '() n) (== '() p)]
   [(poso n) (== '() m) (== '() p)]
   [(== '(1) n) (poso m) (== m p)]
   [(>1o n) (== '(1) m) (== n p)]
   [(fresh (x z)
      (== `(0 . ,x) n) (poso x)
      (== `(0 . ,z) p) (poso z)
      (>1o m)
      (*o x m z))]
   [(fresh (x y)
      (== `(1 . ,x) n) (poso x)
      (== `(0 . ,y) m) (poso y)
      (*o m n p))]
   [(fresh (x y)
      (== `(1 . ,x) n) (poso x)
      (== `(1 . ,y) m) (poso y)
      (odd-*o x n m p))]))

(defrel (odd-*o x n m p)
  (fresh (q)
    (bound-*o q p n m)
    (*o x m q)
    (+o `(0 . ,q) m p)))

(defrel (bound-*o q p n m)
  (conde
   [(== '() q) (poso p)]
   [(fresh (a0 a1 a2 a3 x y z)
      (== `(,a0 . ,x) q)
      (== `(,a1 . ,y) p)
      (conde
       [(== '() n)
        (== `(,a2 . ,z) m)
        (bound-*o x y z '())]
       [(== `(,a3 . ,z) n)
        (bound-*o x y z m)]))]))
