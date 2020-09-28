#lang scheme

(define (contains? sym l)
  (if (null? l) false
      (if (equal? sym (car l)) #t
          (contains? sym (cdr l)))))

(define (cons-without-duplicates s l)
  (if (contains? s l) l (cons s l)))

(define (merge-without-duplicates l1 l2)
  (if (null? l1) l2
      (merge-without-duplicates (cdr l1) (cons-without-duplicates (car l1) l2))))

(define (merge fb1 fb2)
  (list (merge-without-duplicates (car fb1) (car fb2))
        (merge-without-duplicates (cadr fb1) (cadr fb2))))

(define (add-binding binding result)
  (if (contains? binding (cadr result))
            (list (cons binding (car result))
            (remove binding (cadr result)))
            (list (car result) (cadr result))))

(define (abstraction? exp) (eq? (car exp) 'lambda))

(define (get-binding lambda-exp) (if (null? (cadr lambda-exp)) '() (caadr lambda-exp)))

(define (get-exp lambda-exp) (caddr lambda-exp))

(define (trace exp)
  (if (symbol? exp) (list '() (list exp))
      (if (abstraction? exp) (add-binding (get-binding exp) (trace (get-exp exp)))
          (merge (trace (car exp)) (trace (cadr exp))))))

(define (free-vars exp) (cadr (trace exp)))

(define (bound-vars exp) (car (trace exp)))

(define (free? sym exp) (contains? sym (free-vars exp)))
(define (bound? sym exp) (contains? sym (bound-vars exp)))

(equal? (free? 'x 'x) #t)
(equal? (bound? 'x '(lambda (x) x)) #t)
(equal? (free? 'q '(lambda (y) y)) #f)
(equal? (bound? 'x '(lambda (y) (lambda (z) (lambda (x) x)))) #t)