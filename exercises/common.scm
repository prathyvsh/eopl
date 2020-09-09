#lang scheme

(define (if-exp? exp) (eq? (first exp) 'if))
(define (lambda-exp? exp) (eq? (first exp) 'lambda))
(define (get-bindings exp) (cadr exp))
(define (get-exp exp) (caddr exp))


(define (contains? sym los)
  (if (null? los) #f
      (if (eq? sym (car los)) #t
          (contains? sym (cdr los)))))