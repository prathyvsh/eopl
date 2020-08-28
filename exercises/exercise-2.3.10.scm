#lang scheme
(require racket/trace)

(define (if-exp? exp) (eq? (first exp) 'if))
(define (lambda-exp? exp) (eq? (first exp) 'lambda))
(define (get-bindings exp) (cadr exp))
(define (get-exp exp) (caddr exp))

(define (get-symbol sdp) (car sdp))
(define (get-depth sdp) (cdar sdp))
(define (get-length sdp) (cdadr sdp))

(define (find-index-helper sym los)
(if (equal? sym (car los)) 0
          (if (find-index sym (cdr los))
              (+ 1 (find-index sym (cdr los)))
              #f)))
          
(define (find-index sym los)
  (if (null? los) #f
      (find-index-helper sym los)))

(trace find-index)

;; sdp: (list <symbol> (list <depth> <height>))
(define (form-lambda-exp bindings sdp)
  (if (find-index (get-symbol sdp) bindings)
      (list (list 'lambda bindings (list (get-symbol sdp) (list 0 (find-index (get-symbol sdp) bindings)))) 0)
      (list 'lambda bindings sdp (list 0))))

;; <exp> ::= <varref> | (if <exp> <exp> <exp>) | (lambda ({<var>}*) <exp>) | ({<exp>})+
(define (lexical-address-traverse exp)
  (if (symbol? exp) (list exp (list 0 0))
      (if (if-exp? exp) (list 'if (map lexical-address (cdr exp)))
          (if (lambda-exp? exp) (form-lambda-exp (get-bindings exp) (lexical-address (get-exp exp)))
              (map lexical-address exp)))))

(define (append-with-colon a b)
  (apply append a (list ':) b))

(define (lexical-address exp)
  (lexical-address-traverse exp))

(equal? (lexical-address 'a) '(a : 0 0))
(equal? (lexical-address '(lambda (x) x)) '(lambda (x) '(x : 0 0)))
(equal? (lexical-address '(lambda (x) y)) '(lambda (y) '(y : 0 0)))
(equal? (lexical-address '(x y)) '((x : 0 0) (y : 0 1)))
(equal? (lexical-address '(lambda (x y) (x y))) '(lambda (x) '((x : 0 0) (y : 0 1))))
(equal? (lexical-address '(lambda (x) (lambda (y) y))) '(lambda (x) (lambda (y) '(y : 1 0))))
(equal? (lexical-address '(lambda (x y) ((lambda (a) (x (a y))) x))) '(lambda (x y) ((lambda (a) ((x : 1 0) ((a: 0 0) (y: 1 1))))
                                                                                     (x : 0 0))))