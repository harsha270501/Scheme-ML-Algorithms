#lang racket
(provide (all-defined-out))
(define (make-dataframe head data)
  (cons head data))

(define (get-head df)
  (car df))

(define (get-values df)
  (cdr df))

(define (coln-access tdarr index)
  (if (null? tdarr) '()
      (cons (list-ref (car tdarr) index) (coln-access (cdr tdarr) index))))

(define (row-access tdarr index)
  (if (null? tdarr) '()
      (list-ref tdarr index))
)

(define (slice arr indices)
  (let ((start (car indices))
        (end (cdr indices)))
    (if (or (= start end) (> start end))
        '()
        (let((new-start (+ start 1)))(cons (list-ref arr start) (slice arr (list new-start end))))
    )
  )
)

(define (slice-row df indices)
  (cons (get-head df) (slice (get-values df) indices)))

(define (slice-coln df indices)
  (define (slice-coln-helper df indices)
     (if (null? df) '()
      (cons (slice (car df) indices) (slice-coln-helper (cdr df) indices))
     )
  )
  (cons (slice (get-head df) indices) (slice-coln-helper (get-values df) indices))
)


(define (select arr indices)
  (cond ((null? arr) '())
        ((null? indices) '())
        (else (cons (list-ref arr (car indices)) (select arr (cdr indices)))))
)

(define (select-row df indices)
  (cons (get-head df) (select (get-values df) indices)))

(define (select-coln df indices)
  (define (select-coln-helper df indices)
     (if (null? df) '()
      (cons (select (car df) indices) (select-coln-helper (cdr df) indices))
     )
  )
  (cons (select (get-head df) indices) (select-coln-helper (get-values df) indices))
)  
