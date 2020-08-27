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

(define (if-exp? exp) (eq? (car exp) 'if))

;; exp : <varref> | (if <pred> <exp1> <exp2>) |  (lambda (<var>) <exp>) | (<exp> <exp>)
(define (trace-with-if exp)
  (if (symbol? exp) (list '() (list exp))
      (if (if-exp? exp) (merge (merge (trace-with-if (cadr exp)) (trace-with-if (caddr exp))) (trace-with-if (cadddr exp)))
          (if (abstraction? exp) (add-binding (get-binding exp) (trace-with-if (get-exp exp)))
              (merge (trace-with-if (car exp)) (trace-with-if (cadr exp)))))))

(define (free-vars-with-if exp) (cadr (trace-with-if exp)))
(define (bound-vars-with-if exp) (car (trace-with-if exp)))

(define (set-equal? x y)
  (if (equal? (length x) (length y))
      (if (and (empty? x) (empty? y)) #t
          (if (or (empty? x) (empty? y)) #f
              (andmap (lambda (n) (contains? n y)) x)))
      #f))

(set-equal? (free-vars-with-if '(lambda (x) x)) '())
(set-equal? (bound-vars-with-if '(lambda (x) x)) '(x))
(set-equal? (free-vars-with-if '(if ((lambda (x) x) y) w z)) '(y w z))
(set-equal? (bound-vars-with-if '(lambda (x y) (if ((lambda (y) y) y) (lambda (z) x) (lambda (w) w)))) '(x y w))
(set-equal? (free-vars-with-if '(if ((lambda () (a b)) (lambda (a) a)) (lambda (x) x) (lambda () x))) '(a b x))
(set-equal? (bound-vars-with-if '(if ((lambda (x) x) x) (lambda (z) z) (x (y z)))) '(x z))