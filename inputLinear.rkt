#lang racket
(require "./data/dataframe.rkt")
(require "./data/dictionary.rkt")
(require "./abstract_procedures/evaluatealgo.rkt")
(require "./abstract_procedures/traintestsplit.rkt")
(require "./ML_Algos/Linear_Regression.rkt")
(require "./ML_Algos/Naive_Bayes.rkt")
(require "./abstract_procedures/accuracy.rkt")
(require racket/trace)
(require csv-reading)

(define csv-reader
  (make-csv-reader-maker
   '((separator-chars            #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define next-row
  (csv-reader (open-input-file "Housing.csv")))

(define (convert-dtype row)
     (cond ((null? row) '())
           ((string->number (car row))
            (cons (string->number (car row)) (convert-dtype (cdr row))))
           (else (cons (car row) (convert-dtype (cdr row)))))
)

(define (read-values)
  (let ((new-row (next-row)))
    (if (null? new-row) '()
        (cons (convert-dtype new-row) (read-values))))
)

(define head (next-row))
(define data (read-values))
(define df (make-dataframe head data))

(define acc (evaluate-algorithm df train-test-split 70 simple_linear_regression rmse '()))