#lang scheme

(define (cons-without-duplicates s l)
  (if (contains? s l) l (cons s l)))

(define (merge-without-duplicates l1 l2)
  (if (null? l1) l2
      (merge-without-duplicates (cdr l1) (cons-without-duplicates (car l1) l2))))

(define (get-var lambda-exp)
  (caadr lambda-exp))

(define (get-exp lambda-exp)
  (caddr lambda-exp))

(define (contains? sym los)
  (if (null? los) false
      (if (equal? (car los) sym) true
          (contains? sym (cdr los)))))

(define (isbound? var exp)
  (if (symbol? exp) (equal? var exp)
      (contains? var (free-vars exp))))

;; In each step, you need to know the bindings so far. Is this a necessity?
;; I think yes, because it is the context in which an expression that is embedded that
;; determines whether a variable is free or not, adding a lambda function as a context
;; with a certain binding can have a change in meaning for a variable even if it is deeply
;; nested within other expressions.
(define (free-vars-in-lambda-exp lexp bindings)
  (free-vars (get-exp lexp) (cons (get-var lexp))))

(define (free-vars-in-application exp)
  (merge-without-duplicates (free-vars (car exp)) (free-vars (cadr exp))))

;; exp : <varref> | (lambda (<var>) <exp>) | (<exp> <exp>)
(define (free-vars-helper exp bindings)
  (if (symbol? exp) (list exp)
      (if (eq? (first exp) 'lambda) (free-vars-in-lambda-exp exp bindings)
          (free-vars-in-application exp bindings))))


(define (free-vars exp) (free-vars-helper exp '()))

(equal? (free-vars 'a) '(a))
(equal? (free-vars '(lambda (a) a)) '())
(equal? (free-vars '((lambda (a) a) b)) '(b))
(equal? (free-vars '(lambda (x) (lambda (y) (x y)))) '())
(equal? (free-vars '(lambda (x) (lambda (y) p))) '(x y))