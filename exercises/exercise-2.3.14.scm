#lang scheme

;; Helpers

(define (if-exp? exp) (eq? (first exp) 'if))
(define (lambda-exp? exp) (eq? (first exp) 'lambda))
(define (get-bindings exp) (cadr exp))
(define (get-exp exp) (caddr exp))

(define (contains? sym los)
  (if (null? los) #f
      (if (eq? sym (car los)) #t
          (contains? sym (cdr los)))))


(define (rename-var var v1 v2 free-vars bound-vars)
  (if (contains? v1 free-vars) #f
          (if (contains? var bound-vars)
              (list var free-vars bound-vars)
              (if (eq? var v2)
                  (list v1 free-vars bound-vars)
                  (list var (cons var free-vars) bound-vars)))))

(define (build-list car-result rest-of-list bound-vars v1 v2)
  (list (cons (car car-result)
              (car (rename-expr-list rest-of-list v1 v2 (cadr car-result) bound-vars)))
  (cadr (rename-expr-list rest-of-list v1 v2 (cadr car-result) bound-vars)) bound-vars))

(define (rename-expr-list l v1 v2 free-vars bound-vars)
  (if (null? l) (list '() free-vars bound-vars)
      (build-list (rename-helper (car l) v1 v2 free-vars bound-vars) (cdr l) bound-vars v1 v2)))

(define (build-lambda-expr exp v1 v2 result bound-vars)
(list (list 'lambda (get-bindings exp) (car result))
      (cadr result) bound-vars))

(define (rename-helper exp v1 v2 free-vars bound-vars)
  (if (symbol? exp) (rename-var exp v1 v2 free-vars bound-vars)
      (if (lambda-exp? exp) (build-lambda-expr exp v1 v2
                                               (rename-helper (get-exp exp) v1 v2 free-vars (append bound-vars (get-bindings exp))) bound-vars)
          (if (if-exp? exp) (list 'if (rename-expr-list (cdr exp) v1 v2 free-vars bound-vars))
              (rename-expr-list exp v1 v2 free-vars bound-vars)))))

  
(define (rename exp v1 v2)
  (car (rename-helper exp v1 v2 '() '())))

(require racket/trace)
(trace rename-helper)
(trace rename-var)


;; No op
(equal? (rename 'x 'y 'z) 'x)
(equal? (rename 'a 'b 'a) 'b)
(equal? (rename '(a b) 'x 'z) '(a b))

;; No ops in Lambda
(equal? (rename '(lambda (a) a) 'b 'a) '(lambda (a) a))
(equal? (rename '(lambda (b) a) 'a 'b) '(lambda (b) a))
(equal? (rename '(lambda (a) b) 'z 'b) '(lambda (a) z))

;; Nested lambda no-op
(equal? (rename '(lambda (a) (lambda (b) b)) 'a 'b) '(lambda (a) (lambda (b) b)))
(equal? (rename '(lambda (a) (lambda (b) (a b))) 'b 'a) '(lambda (a) (lambda (b) (a b))))