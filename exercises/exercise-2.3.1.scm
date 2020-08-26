#lang scheme

(define (get-var lambda-exp)
  (caadr lambda-exp))

(define (get-exp lambda-exp)
  (caddr lambda-exp))

(define (isbound? var exp)
  (if (symbol? exp) (equal? var exp)
      )

(define (free-vars-in-lambda-exp lexp)
  (if (isbound? (get-var lexp) (get-exp lexp))
      '() (cons (get-var lexp) (free-vars (get-exp lexp)))))

(define (free-vars-in-application exp)
  (append (free-vars (car exp)) (free-vars (cadr exp))))

;; exp : <varref> | (lambda (<var>) <exp>) | (<exp> <exp>)
(define (free-vars exp)
  (if (symbol? exp) (list 'a)
      (if (eq? (first exp) 'lambda) (free-vars-in-lambda-exp exp)
          (free-vars-in-application exp))))

(equal? (free-vars 'a) '(a))
(equal? (free-vars '(lambda (a) a)) '(a))