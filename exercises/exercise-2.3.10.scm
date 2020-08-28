#lang scheme

(define (if-exp? exp) (eq? (first exp) 'if))
(define (lambda-exp? exp) (eq? (first exp) 'lambda))
(define (get-bindings exp) (cadr exp))
(define (get-exp exp) (caddr exp))

;; <exp> ::= <varref> | (if <exp> <exp> <exp>) | (lambda ({<var>}*) <exp>) | ({<exp>})+
(define (lexical-address-traverse exp)
  (if (symbol? exp) (list (list exp) (list 0 0))
      (if (if-exp? exp) (list 'if (map lexical-address (cdr exp)))
          (if (lambda-exp? exp) (list (list 'lambda (get-bindings exp)) (lexical-address (get-exp exp)))
              (map lexical-address exp)))))

(define (append-with-colon a b)
  (apply append a (list ':) b))

(define (lexical-address exp)
  (apply append-with-colon (lexical-address-traverse exp)))

(equal? (lexical-address 'a) '(a : 0 0))
(equal? (lexical-address '(lambda (x) x)) '(lambda (x) '(x : 0 0)))
(equal? (lexical-address '(lambda (x) (lambda (y) y))) '(lambda (x) (lambda (y) '(y : 1 0))))
(equal? (lexical-address '(lambda (x y) ((lambda (a) (x (a y))) x))) '(lambda (x y) ((lambda (a) ((x : 1 0) ((a: 0 0) (y: 1 1))))
                                                                                     (x : 0 0))))