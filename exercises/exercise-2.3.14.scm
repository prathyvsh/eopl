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
          (if (contains? var bound-vars)
              (list var free-vars bound-vars)
              (if (or (contains? v1 free-vars)
                      (eq? var v1)) #f
                  (if (eq? var v2)
                      (list v1 free-vars bound-vars)
                      (list var (cons var free-vars) bound-vars)))))

(define (build-list car-result rest-of-list bound-vars v1 v2)
  (if (or (eq? car-result #f)
       (eq? (rename-expr-list rest-of-list v1 v2 (cadr car-result) bound-vars) #f)) #f
  (list (cons (car car-result)
              (car (rename-expr-list rest-of-list v1 v2 (cadr car-result) bound-vars)))
  (cadr (rename-expr-list rest-of-list v1 v2 (cadr car-result) bound-vars)) bound-vars)))

(define (rename-expr-list l v1 v2 free-vars bound-vars)
  (if (null? l) (list '() free-vars bound-vars)
      (if (eq? (rename-helper (car l) v1 v2 free-vars bound-vars) #f) #f
      (build-list (rename-helper (car l) v1 v2 free-vars bound-vars) (cdr l) bound-vars v1 v2))))

(define (build-if-expr result)
  (list (cons 'if (car result))
              (cadr result) (caddr result)))

(define (build-lambda-expr exp v1 v2 result bound-vars)
  (if (eq? result #f) #f
(list (list 'lambda (get-bindings exp) (car result))
      (cadr result) bound-vars)))

(define (rename-helper exp v1 v2 free-vars bound-vars)
  (if (symbol? exp) (rename-var exp v1 v2 free-vars bound-vars)
      (if (lambda-exp? exp) (build-lambda-expr exp v1 v2
                                               (rename-helper (get-exp exp) v1 v2 free-vars (append bound-vars (get-bindings exp))) bound-vars)
          (if (if-exp? exp) (build-if-expr (rename-expr-list (cdr exp) v1 v2 free-vars bound-vars))
              (rename-expr-list exp v1 v2 free-vars bound-vars)))))

  
(define (rename exp v1 v2)
  (if (rename-helper exp v1 v2 '() '())
  (car (rename-helper exp v1 v2 '() '()))
  #f))

(require racket/trace)
; (trace rename-helper)
; (trace rename-var)


;; No op
(equal? (rename 'x 'y 'z) 'x)
(equal? (rename '(a b) 'x 'z) '(a b))
(equal? (rename '(lambda (a) a) 'b 'a) '(lambda (a) a))
(equal? (rename '(lambda (a) b) 'z 'b) '(lambda (a) z))
(equal? (rename '(lambda (a) (lambda (b) b)) 'a 'b) '(lambda (a) (lambda (b) b)))
(equal? (rename '(lambda (a) a) 'a 'm) '(lambda (a) a))
(equal? (rename '(lambda (a) (lambda (b) (a b))) 'c 'b) '(lambda (a) (lambda (b) (a b))))


;; False
(equal? (rename 'a 'a 'b) #f)
(equal? (rename '(lambda (a) (x y)) 'x 'y) #f)
(equal? (rename '(a b) 'a 'b) #f)

; Single variable
(equal? (rename 'a 'b 'a) 'b)

; Lambda
(equal? (rename '(lambda (a) b) 'c 'b) '(lambda (a) c))
(equal? (rename '(lambda (b) (b a)) 'c 'a) '(lambda (b) (b c)))
(equal? (rename '(lambda (a) (x x)) 'y 'x) '(lambda (a) (y y)))

;; Nested lambda
(equal? (rename '(lambda (a) (lambda (b) (lambda (c) (x y z)))) 'w 'x)
        '(lambda (a) (lambda (b) (lambda (c) (w y z)))))
(equal? (rename '(lambda (a) (lambda (b) (c (b (c a))))) 'd 'c)
        '(lambda (a) (lambda (b) (d (b (d a))))))

;; If
(equal? (rename '(if a b c) 'd 'b) '(if a d c))

;; Application
(equal? (rename '((lambda (x) x) x) 'y 'x) '((lambda (x) x) y))
(equal? (rename '(a b c d) 'm 'c) '(a b m d))

;; Heterogenous expressions
(equal? (rename '(a (if b c d) (lambda (y) c)) 'm 'c)
        '(a (if b m d) (lambda (y) m)))
(equal? (rename '(a b c (lambda (b) b)) 'y 'b)
        '(a y c (lambda (b) b)))
(equal? (rename '(if (lambda (c) (a c)) (a b) (a c)) 'x 'c)
        '(if (lambda (c) (a c)) (a b) (a x)))

(equal? (rename '(if (lambda (c) (a c)) (a b) (a c)) 'x 'a)
        '(if (lambda (c) (x c)) (x b) (x c)))