#lang racket
(define (make-dict keys values)
  (if (or (null? keys) (null? values))
      '()
      (cons (cons (car keys) (car values)) (make-dict (cdr keys) (cdr values)))))

(define (insert dict key value)
  (cond ((null? dict) '())
        ((eq? (caar dict) key)
         (cons (cons key value) (cdr dict)))
        (else (cons (car dict) (insert (cdr dict) key value)))))

(define (remove dict key)
  (cond ((null? dict) '())
        ((eq? (caar dict) key)
         '())
        (else (cons (car dict) (remove (cdr dict) key)))))

(define (getval dict key)
  (cond ((null? dict) '())
        ((eq? (caar dict) key)
         (car dict))
        (else (getval (cdr dict) key))))

