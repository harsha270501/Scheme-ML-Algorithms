#lang racket
(provide (all-defined-out))
(require "../mathfns.rkt")

(define (coefficientb1 x y)
  (let ((x_mean (mean x)) (y_mean (mean y)))
  (/ (covariance x x_mean y y_mean)(variance x x_mean))))

(define (coefficientb0 x y)
  (let ((x_mean (mean x)) (y_mean (mean y)))
  (- y_mean (* (coefficientb1 x y) x_mean))))

(define (linear_regression b0 b1 x_test)
  (if (null? x_test) '()
      (cons (+ b0 (* b1 (car x_test))) (linear_regression b0 b1 (cdr x_test)))
     ))

(define (simple_linear_regression x_train y_train x_test)
  (define b0 (coefficientb0 x_train y_train))
  (define b1 (coefficientb1 x_train y_train))
  (define pred (linear_regression b0 b1 x_test))
  pred)
  
  
     