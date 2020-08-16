#lang scheme

(define compose
  (lambda (f)
    (lambda (g)
      (lambda (x)
        (g (f x))))))

(define double (lambda (x) (* 2 x)))

(define half (lambda (x) (/ x 2)))

(((compose double) half) 3)

;; You can use the curried version to create partially composed
;; functions which can be composed with multiple other functions.
;; In the same way double is composed with half, it could be
;; composed with a triple.