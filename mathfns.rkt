#lang racket
(provide (all-defined-out))
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
