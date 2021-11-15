#lang racket
(require "../data/dataframe.rkt")
(require "accuracy.rkt")
(require "extractclass.rkt")
(require racket/trace)
(provide (all-defined-out))
(define (evaluate-algorithm data splitfn split algo acc args)
  (define traintest (splitfn (get-values data) split))
  
  (define traindata (car traintest))
  (define testdata (cadr traintest))
  
  (define x_train (coln-access (slice-coln traindata (cons 0 (- (number-colns traindata) 1))) 0))
  (define y_train (coln-access (select-coln traindata (list (- (number-colns traindata) 1))) 0))

  (define x_test (coln-access (slice-coln testdata (cons 0 (- (number-colns testdata) 1))) 0))
  (define y_test (coln-access (select-coln testdata (list (- (number-colns testdata) 1))) 0))

  (let ((pred (algo x_train y_train x_test)))
    (acc pred y_test)
  )
  
)

(trace-define (evaluate-algo data splitfn split algo acc args)
  (define traintest (splitfn (get-values data) split))
  
  (define traindata (car traintest))
  (define testdata (cadr traintest))
   (let ((pred (algo traindata testdata args))
         (y_test (get-all-class testdata)))
    (acc pred y_test)
  )
)

(trace-define (eval-algo data splitfn algo acc args)
  (define traintest (splitfn (get-values data)))
  
  (define traindata (car traintest))
  (define testdata (cadr traintest))
  (algo traindata testdata args)
)


