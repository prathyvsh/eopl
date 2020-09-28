#lang scheme
(require racket/trace)

(define (if-exp? exp) (eq? (first exp) 'if))
(define (lambda-exp? exp) (eq? (first exp) 'lambda))
(define (get-bindings exp) (cadr exp))
(define (get-exp exp) (caddr exp))
(define (dp? exp) (eq? (car exp) ':))

(define (find-binding bindings pos)
  (if (null? bindings) #f
      (if (equal? pos 0) (car bindings)
          (find-binding (cdr bindings) (- pos 1)))))

(define (match-binding bindings dep pos)
  (if (null? bindings) #f
      (if (eq? dep 0) (find-binding (car bindings) pos)
          (match-binding (cdr bindings) (- dep 1) pos))))

(equal? (match-binding '((a)) 0 0) 'a)
(equal? (match-binding '((a) (x y)) 1 1) 'y)
(equal? (match-binding '((a) (x y)) 1 0) 'x)


; (trace find-binding)

(define (build-if-application result)
  (cons 'if result))  

(define (build-list loe bindings) (map (lambda (n) (un-lexical-address-helper n bindings)) loe))

(define (build-lambda-exp bindings result)
  (list 'lambda bindings result))

(define (un-lexical-address-helper exp bindings)
  (if (dp? exp) (match-binding bindings (cadr exp) (caddr exp))
      (if (if-exp? exp) (build-if-application (build-list (cdr exp) bindings))
          (if (lambda-exp? exp)
              (build-lambda-exp (get-bindings exp) (un-lexical-address-helper (get-exp exp) (cons (get-bindings exp) bindings)))
              (build-list exp bindings)))))

(define (un-lexical-address exp)
  (un-lexical-address-helper exp '()))

(equal? (un-lexical-address '(lambda (a) (: 0 0))) '(lambda (a) a))
(equal? (un-lexical-address '(lambda (a b) (: 0 1))) '(lambda (a b) b))
(equal? (un-lexical-address '(lambda (a b) ((: 0 0) (: 0 1)))) '(lambda (a b) (a b)))
(equal? (un-lexical-address '(lambda (a) (lambda (b) (: 1 0)))) '(lambda (a) (lambda (b) a)))
(equal? (un-lexical-address '(lambda (a) (lambda (b c) ((: 1 0) (: 0 0) (: 0 1)))))
        '(lambda (a) (lambda (b c) (a b c))))
(equal? (un-lexical-address '(lambda (eq? cons)
                               (lambda (a b c)
                                 (if ((: 1 0) (: 0 1) (: 0 2))
                                     ((lambda (c)
                                        ((: 2 1) (: 1 0) (: 0 0)))
                                      (: 0 0))
                                     (: 0 1))))) '(lambda (eq? cons)
                                                    (lambda (a b c)
                                                      (if (eq? b c)
                                                       ((lambda (c) (cons a c)) a)
                                                       b))))
