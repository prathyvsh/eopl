#lang scheme

;; Helpers

(define (if-exp? exp) (eq? (first exp) 'if))
(define (lambda-exp? exp) (eq? (first exp) 'lambda))
(define (get-bindings exp) (cadr exp))
(define (get-exp exp) (caddr exp))

(define (contains? sym los)
  (if (null? los) #f
      (if (eq? sym (car los)) #t
          (contains? sym (cdr los)))))


(define (rename-var var v1 v2 free-vars bound-vars)
  (if (contains? free-vars v1) #f
      (if (equal? var v1) (list v1 free-vars)
          var)))

(define (rename-helper exp v1 v2 free-vars bound-vars)
  (if (symbol? exp) (rename-var exp v1 v2 free-vars bound-vars)
      
  )

  
(define (rename exp v1 v2)
  (rename-helper exp v1 v2 free-vars bound-vars))