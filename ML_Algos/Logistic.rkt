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
  (/ 1 (+ 1 (exp (- (predict-main)))))
  )



(define (train-coef train lrate nepochs)

  (define (update-coef wt yhat row err)
    (if (or (null? row) (null? wt))
        '()
        (cons (+ (car wt) (* err lrate yhat (- 1 yhat) (car row))) (update-coef (cdr wt) yhat (cdr row) err))))
  
  (define (init-coef l)
    (if (= l 0) '()
        (cons 0 (init-coef (- l 1)))))
  
  (define coef (init-coef (length (car train))))

  (define (train-epoch wt np)
    (if (= np 0) wt
        (train-epoch (train-epoch-all-rows train wt) (- np 1))))
  
  (define (train-epoch-row row wt)
    (let ((yhat (predict row wt))
          (class (get-class row)))
      (let ((err (- class yhat)))
        (cons (* (car wt) lrate err yhat (- 1 yhat)) (update-coef (cdr wt) yhat row err))
        )))

  
  (define (train-epoch-all-rows df wt)
    (if (null? df) wt
        (train-epoch-all-rows (cdr df) (train-epoch-row (car df) wt))))

  (train-epoch coef nepochs)
)

(trace-define (logistic train x_test args)
  (trace-define (predict-test wts xtest)
    (if (null? xtest) '()
        (cons (predict (car xtest) wts) (predict-test wts (cdr xtest)))))
  
  (let ((trained_coef (train-coef train (car args) (cdr args))))
    (predict-test trained_coef x_test)))