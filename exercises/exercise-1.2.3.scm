#lang scheme

(define v1 (vector (cons 1 2) 3))
(define v2 (vector 'a v1))

(equal? v2 #(a #((1 . 2) 3)))

(define v3 '#(a #((1 . 2) 3)))

(eq? (eq? v2 v3) #f)

(eq? (eq? v1 (vector-ref v2 1)) #t)

(eq? (eq? (vector-ref v1 0)
     (vector-ref (vector-ref v2 1) 0)) #t)