#lang racket
(require "dataframe.rkt")
(require csv-reading)

(define csv-reader
  (make-csv-reader-maker
   '((separator-chars            #\,)
     (strip-leading-whitespace?  . #t)
     (strip-trailing-whitespace? . #t))))

(define next-row
  (csv-reader (open-input-file "Iris.csv")))

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
