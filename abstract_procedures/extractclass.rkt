#lang racket
(provide (all-defined-out))
(define (get-class row)
  (list-ref row (- (length row) 1)))

(define (get-all-class test)
  (if (null? test) '()
      (cons (get-class (car test)) (get-all-class (cdr test)))))