#lang scheme

(define cell-tag "cell")

(define make-cell (lambda (x) (vector cell-tag x)))

(define cell? (lambda (x) (if (vector? x) (if (= (vector-length x) 2)
                                              (eq? (vector-ref x 0) cell-tag) #f) #f)))

(define cell-ref (lambda (x) (if (cell? x) (vector-ref x 1)
                                 (error "Invalid argument to cell-ref:" x))))

(define c (make-cell 5))

(equal? c #("cell" 5))

(eq? (cell? c) #t)

(equal? (cell-ref c) 5)