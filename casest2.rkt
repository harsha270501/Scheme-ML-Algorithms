#lang racket
(define (sum val)
  (if (null? val) 0
      (+ (car val) (sum (cdr val)))))
(define (size val)
  (if (null? val) 0
      (+ 1 (size (cdr val)))))
(define (square x)
  (* x x))
(define (mean val)
  (/ (sum val) (size val)))
(define (variance val mn)
   (if (null? val) 0
      (+ (square (- (car val) mn)) (variance (cdr val) mn))))
(define (covariance x x_mean y y_mean)
  (if (null? x) 0
      (+ (* (- (car x) x_mean) (- (car y) y_mean)) (covariance (cdr x) x_mean (cdr y) y_mean))))
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
(define (sum_error y_test pred)
  (if (null? y_test) 0
      (+ (square (- (car pred) (car y_test))) (sum_error (cdr y_test) (cdr pred)))
     ))
(define (rmse y_test pred)
  (sqrt (/ (sum_error y_test pred) (size y_test))))
(define (simple_linear_regression x_train y_train x_test y_test)
  (define b0 (coefficientb0 x_train y_train))
  (define b1 (coefficientb1 x_train y_train))
  (define pred (linear_regression b0 b1 x_test))
  (rmse y_test pred))
  
  
  
(simple_linear_regression (list 1 2 3) (list 1 2 3) (list 1 2 3) (list 1 2 3))
  