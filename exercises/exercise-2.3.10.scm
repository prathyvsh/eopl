#lang scheme
(require racket/trace)

;; Approach 1
(define (find-pos-helper sym los)
(if (equal? sym (car los)) 0
          (if (find-pos sym (cdr los))
              (+ 1 (find-pos sym (cdr los)))
              #f)))
          
(define (find-pos sym los)
  (if (null? los) #f
      (find-pos-helper sym los)))

;; lob: <sym> | <los>
;; los : ({<sym>}*)
(define (find-dp sym lob))

; (find-dp 'x '(x))
; (find-dp 'x '(()))

#|

(define (if-exp? exp) (eq? (first exp) 'if))
(define (lambda-exp? exp) (eq? (first exp) 'lambda))
(define (get-bindings exp) (cadr exp))
(define (get-exp exp) (caddr exp))

(define (get-symbol sdp) (car sdp))
(define (get-depth sdp) (cdar sdp))
(define (get-length sdp) (cdadr sdp))


(trace find-index)

(define (assign-pos lexp pos)
  (if pos (list 'lambda bindings (list pos depth))
      (list 'lambda bindings (
  
;; sdp: (list <symbol> (list <depth> <height>))
(define (form-lambda-exp bindings sdp depth)
  (if (find-index (get-symbol sdp) bindings)
      (list (list 'lambda bindings (list (get-symbol sdp) (list depth (find-index (get-symbol sdp) bindings)))) (+ depth 1))
      (list 'lambda bindings sdp (list 0))))

;; <exp> ::= <varref> | (if <exp> <exp> <exp>) | (lambda ({<var>}*) <exp>) | ({<exp>})+
(define (lexical-address-tr exp depth)
  (if (symbol? exp) (list exp ': (list d 0))
      (if (if-exp? exp) (list 'if (map lexical-address (cdr exp)))
          (if (lambda-exp? exp) (form-lambda-exp (get-bindings exp) (lexical-address (get-exp exp)) depth)
              (map lexical-address exp)))))

(define (append-with-colon a b)
  (apply append a (list ':) b))

(define (lexical-address exp)
  (lexical-address-tr exp 0))

(equal? (lexical-address 'a) '(a : 0 0))
(equal? (lexical-address '(lambda (x) x)) '(lambda (x) '(x : 0 0)))
(equal? (lexical-address '(lambda (x) y)) '(lambda (y) '(y : 0 0)))
(equal? (lexical-address '(x y)) '((x : 0 0) (y : 0 1)))
(equal? (lexical-address '(lambda (x y) (x y))) '(lambda (x) '((x : 0 0) (y : 0 1))))
(equal? (lexical-address '(lambda (x) (lambda (y) y))) '(lambda (x) (lambda (y) '(y : 1 0))))
(equal? (lexical-address '(lambda (x y) ((lambda (a) (x (a y))) x))) '(lambda (x y) ((lambda (a) ((x : 1 0) ((a: 0 0) (y: 1 1))))
                                                                                     (x : 0 0))))
|#