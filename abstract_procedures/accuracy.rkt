#lang racket
(require "../mathfns.rkt")
(provide (all-defined-out))
(define (sum_error y_test pred)
  (if (null? y_test) 0
      (+ (square (- (car pred) (car y_test))) (sum_error (cdr y_test) (cdr pred)))
     ))

(define (rmse y_test pred)
  (sqrt (/ (sum_error y_test pred) (length y_test))))

(define (acc_metric actual pred)
  (define (find-correct actual pred)
    (cond ((or (null? actual) (null? pred)) 0)
          ((eq? (car actual) (car pred)) (+ 1 (find-correct (cdr actual) (cdr pred))))
          (else (find-correct (cdr actual) (cdr pred)))))
  (let ((correct (find-correct actual pred)))
       (* (/ correct (length actual)) 100)))