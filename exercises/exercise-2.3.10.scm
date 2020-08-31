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

(define (search-list sym lob)
  (if (find-pos sym (car lob))
      (list 0 (find-pos sym (car lob)))
      (find-dp sym (cdr lob))))

(define (increment-depth result)
  (if result (list (+ (car result) 1) (cadr result))
      result))

(define (increment-pos result)
  (if (and result (= (car result) 0)) (list (car result) (+ (cadr result) 1)) result))

;; lob: <sym> | ({<lob>}*)
(define (find-dp sym lob)
  (if (equal? lob null) #f
      (if (symbol? (car lob))
          (if (equal? sym (car lob)) (list 0 0)
              (increment-pos (find-dp sym (cdr lob))))
          (if (find-dp sym (car lob))
              (increment-depth (find-dp sym (car lob)))
              (find-dp sym (cdr lob))))))

(equal? (find-dp 'x '(x)) '(0 0))
(equal? (find-dp 'x '(())) #f)
(equal? (find-dp 'x '(x y)) '(0 0))
(equal? (find-dp 'y '(x y)) '(0 1))
(equal? (find-dp 'z '(x y z)) '(0 2))
(equal? (find-dp 'x '(y (x))) '(1 0))
(equal? (find-dp 'x '(a b c (x))) '(1 0))
(equal? (find-dp 'x '(a b (c d x))) '(1 2))
(equal? (find-dp 'x '(a b (c d (x)))) '(2 0))



(define (if-exp? exp) (eq? (first exp) 'if))
(define (lambda-exp? exp) (eq? (first exp) 'lambda))
(define (get-bindings exp) (cadr exp))
(define (get-exp exp) (caddr exp))

(define (get-symbol sdp) (car sdp))
(define (get-depth sdp) (cdar sdp))
(define (get-length sdp) (cdadr sdp))

(define (add-bindings bindings binding-store)
  (if (empty? binding-store) bindings (append binding-store (list bindings))))

(define (add-exp exp exp-store)
  (if (empty? exp-store) exp (cons exp exp-store)))

(define (build-lambda-exp lexp)
  (list 'lambda (get-bindings lexp)))
;; sdp: (list <symbol> (list <depth> <height>))

(define (form-lambda-exp bindings result)
  (list (list 'lambda bindings (car result)) (cadr result)))

(define (process-lambda-exp exp bindings store free-var-count)
  (form-lambda-exp (get-bindings exp) (lexical-address-tr (get-exp exp) (add-bindings (get-bindings exp) bindings) '() free-var-count)))      

(define (assign-dp exp bindings store free-var-count)
  (if (find-dp exp bindings)
  (add-exp (cons exp (cons ': (find-dp exp bindings))) store)
  (add-exp (cons exp (cons ': (list 0 (+ free-var-count 1)))) store)))

(define (build-application exp bindings store free-var-count)
  (if (null? exp) '()
      (cons (car (lexical-address-tr (car exp) bindings store free-var-count))
       (build-application (cdr exp) bindings store free-var-count))))

(define (process-application exp bindings store free-var-count)
  (list (build-application exp bindings '() free-var-count) free-var-count))

;; <exp> ::= <varref> | (if <exp> <exp> <exp>) | (lambda ({<var>}*) <exp>) | ({<exp>})+
(define (lexical-address-tr exp bindings store free-var-count)
  (if (symbol? exp) (list (assign-dp exp bindings store free-var-count) free-var-count)
      (if (if-exp? exp) (list 'if (map lexical-address (cdr exp)))
          (if (lambda-exp? exp) (process-lambda-exp exp bindings store free-var-count)
              (process-application exp bindings store free-var-count)))))

(define (lexical-address exp)
  (car (lexical-address-tr exp '() '() -1)))

(trace lexical-address-tr)

(equal? (lexical-address 'a) '(a : 0 0))
(equal? (lexical-address '(lambda (a) a)) '(lambda (a) (a : 0 0)))
(equal? (lexical-address '(lambda (x y) y)) '(lambda (x y) (y : 0 1)))
(equal? (lexical-address '(lambda () m)) '(lambda () (m : 0 0)))
(equal? (lexical-address '(lambda (a b c) m)) '(lambda (a b c) (m : 0 0)))
; (equal? (lexical-address '(x y)) '((x : 0 0) (y : 0 1)))
(equal? (lexical-address '(lambda (x y) (x y))) '(lambda (x) '((x : 0 0) (y : 0 1))))
;(equal? (lexical-address '(lambda (x) (lambda (y) y))) '(lambda (x) (lambda (y) '(y : 1 0))))
(equal? (lexical-address '(lambda (x y) ((lambda (a) (x (a y))) x))) '(lambda (x y) ((lambda (a) ((x : 1 0) ((a: 0 0) (y: 1 1)))))))
