#lang racket

(define mpr (mcons 1 (mcons #t "hi")))

(mcons 1 (mcons #t "hi"))

(mcar mpr)

(mcdr mpr)

(mcar (mcdr mpr))

(set-mcdr! mpr 47)
mpr

(set-mcdr! mpr (mcons #t "hi"))
mpr
