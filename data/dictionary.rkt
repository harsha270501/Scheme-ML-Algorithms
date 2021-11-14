#lang racket
(provide (all-defined-out))
(require racket/trace)
(define (make-dict keys values)
  (if (or (null? keys) (null? values))
      '()
      (cons (list (car keys) (car values)) (make-dict (cdr keys) (cdr values)))))

(define (insert dict key value)
  (cond ((null? dict) (cons (list key value) '()))
        ((eq? (caar dict) key)
         (cons (list key value) (cdr dict)))
        (else (cons (car dict) (insert (cdr dict) key value)))))

(trace-define (appendkey dict key value)
  (cond ((null? dict) (cons (cons key (list value)) '()))
        ((eq? (caar dict) key)
         (cons (cons key (cons value (cdar dict))) (cdr dict)))
        (else (cons (car dict) (appendkey (cdr dict) key value)))))

(define (remove dict key)
  (cond ((null? dict) '())
        ((eq? (caar dict) key)
         (remove (cdr dict) key))
        (else (cons (car dict) (remove (cdr dict) key)))))

(define (getval dict key)
  (cond ((null? dict) '())
        ((eq? (caar dict) key)
         (car dict))
        (else (getval (cdr dict) key))))

