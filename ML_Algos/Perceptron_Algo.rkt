#lang racket
(provide (all-defined-out))
(require racket/trace)
(require "../abstract_procedures/extractclass.rkt")
(define (predict row coeff)
  (define (predict-helper row coeff)
    (if (or (null? row) (null? coeff)) 0
        (+ (* (car row) (car coeff)) (predict-helper (cdr row) (cdr coeff)))))
  (define (predict-main)
    (+ (car coeff) (predict-helper (cdr row) (cdr coeff))))
  (if (> (predict-main) 0) 1
      0))



(define (train-weights train lrate nepochs)

  (define (update-weights wt row err)
    (if (or (null? row) (null? wt))
        '()
        (cons (+ (car wt) (* err lrate (car row))) (update-weights (cdr wt) (cdr row) err))))
  
  (define (init-weights l)
    (if (= l 0) '()
        (cons 0 (init-weights (- l 1)))))
  
  (define weights (init-weights (length (car train))))

  (define (train-epoch wt np)
    (if (= np 0) wt
        (train-epoch (train-epoch-all-rows train wt) (- np 1))))
  
  (define (train-epoch-row row wt)
    (let ((pred (predict row wt))
          (class (get-class row)))
      (let ((err (- class pred)))
        (cons (* (car weights) lrate err) (update-weights (cdr weights) row err))
        )))

  
  (define (train-epoch-all-rows df wt)
    (if (null? df) wt
        (train-epoch-all-rows (cdr df) (train-epoch-row (car df) wt))))

  (train-epoch weights nepochs)
)

(trace-define (perceptron train x_test args)
  (trace-define (predict-test wts xtest)
    (if (null? xtest) '()
        (cons (predict (car xtest) wts) (predict-test wts (cdr xtest)))))
  
  (let ((trained_wts (train-weights train (car args) (cdr args))))
    (predict-test trained_wts x_test)))