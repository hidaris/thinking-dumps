#lang typed/racket

(define-type num
  (U Zero
     One_more_than))

(struct Zero () #:transparent)
(struct One_more_than ([v : num])
  #:transparent)
