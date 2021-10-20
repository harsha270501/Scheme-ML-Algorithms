#lang racket
(define (test-split index value dataset)
  (define (test-split-helper data left right)
    (cond ((null? data) (cons left right))
          ((< (list-ref (car data) index) value)
           (test-split-helper (cdr data) (append left row) right))
          (else (test-split-helper (cdr data) left (append right row)))))
  (test-split-helper dataset '() '()))