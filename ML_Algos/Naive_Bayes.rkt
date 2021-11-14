#lang racket
(provide (all-defined-out))
(require "../data/dataframe.rkt")
(require "../data/dictionary.rkt")

(define (get-class row)
  (list-ref row (- (length row) 1)))
    

(define (findclasses df res)
  (if (null? df) res
      (let ((row (car df)))
        (let ((c (get-class row)))
          (cond ((null? res) (findclasses (cdr df) (list c)))
                ((member c res) (findclasses (cdr df) res))
                (else (findclasses (cdr df) (cons c res))))))))

(define (sepclass df)
  (define (sep-class-helper df c)
    (if (null? df) '()
        (let ((class (get-class (car df))))
          (if (eq? class c) (cons (car df) (sep-class-helper (cdr df) c))
              (sep-class-helper (cdr df) c)))))
  (define classes (findclasses (get-values df) '()))
  (define (sepclassmain classes)
    (if (null? classes) '()
      (cons (cons (car classes) (sep-class-helper (get-values df) (cdr classes))) (sepclassmain (cdr classes)))))
  (sepclassmain classes))