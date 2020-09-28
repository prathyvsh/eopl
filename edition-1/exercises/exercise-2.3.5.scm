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

(define (get-bindings exp) (cadr exp))

(define (add-bindings bindings result)
  (if (null? bindings) result
      (add-bindings (cdr bindings) (add-binding (car bindings) result))))

(define (trace-for-multiple-bindings exp)
  (if (symbol? exp) (list '() (list exp))
      (if (abstraction? exp) (add-bindings (get-bindings exp) (trace-for-multiple-bindings (get-exp exp)))
          (merge (trace-for-multiple-bindings (car exp)) (trace-for-multiple-bindings (cadr exp))))))

(define (free-vars-multiple-bindings exp) (cadr (trace-for-multiple-bindings exp)))
(define (bound-vars-multiple-bindings exp) (car (trace-for-multiple-bindings exp)))

(define (set-equal? x y)
  (if (equal? (length x) (length y))
      (if (and (empty? x) (empty? y)) #t
          (if (or (empty? x) (empty? y)) #f
              (andmap (lambda (n) (contains? n y)) x)))
      #f))

(set-equal? (free-vars-multiple-bindings '(lambda (x) y)) '(y))
(set-equal? (free-vars-multiple-bindings '(lambda (y z) x)) '(x))
(set-equal? (free-vars-multiple-bindings '(lambda (x y z) (x (y z)))) '())
(set-equal? (bound-vars-multiple-bindings '(lambda (x y z) (x (y (z y))))) '(x y z))
(set-equal? (bound-vars-multiple-bindings '(lambda (x) y)) '())
(set-equal? (bound-vars-multiple-bindings '(x (lambda (y) (y z)))) '(y))
(set-equal? (bound-vars-multiple-bindings '((lambda (x y z) (x (y z))) (lambda (a b) (a b)))) '(x y z a b))