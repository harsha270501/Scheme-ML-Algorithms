#lang racket
(require "dataframe.rkt")
(define (evaluate-algorithm algo train test)
  (let ((pred (algo train test))
        (actual (select test (- (length (car test)) 1 ))))
    (accuracy pred actual)))
    