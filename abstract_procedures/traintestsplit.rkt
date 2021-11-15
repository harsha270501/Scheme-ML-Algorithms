#lang racket
(require "../data/dataframe.rkt")
(provide (all-defined-out))
(define (train-test-split dataset percentage)
  (define (train-test-helper data train trainlen)
    (if (= trainlen 0)
        (list train data)
        (let ((index (random (length data))))
          (train-test-helper (arr-remove data index) (append train (cons (list-ref data index) '())) (- trainlen 1)))))
  
  (let ((data-copy (get-values dataset))
        (data-head (get-head dataset))
        (train-len (floor (* (/ percentage 100) (length dataset)))))
    (let ((traintest (train-test-helper data-copy '() train-len)))
      traintest)))

(define (train-split dataset)
  (list (cdr dataset) (car dataset)))

(define head (list 'x1 'x2 'x3))
(define data (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 5 8 9)))
(define df (make-dataframe head data))
  