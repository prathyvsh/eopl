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


(define (build-alpha-lexp inner-exp exp v)
  (if inner-exp
    (list 'lambda (cons v (cdr (get-bindings exp))) inner-exp)
    #f))

(define (alpha-convert exp v)
  (build-alpha-lexp (rename (get-exp exp) v (car (get-bindings exp))) exp v))

(equal? (alpha-convert '(lambda (a) a) 'c) '(lambda (c) c))
(equal? (alpha-convert '(lambda (a) (lambda (b) (b a))) 'c)
        '(lambda (c) (lambda (b) (b c))))
(equal? (alpha-convert '(lambda (x) ((lambda (x) x) x)) 'y)
        '(lambda (y) ((lambda (x) x) y)))
(equal? (alpha-convert '(lambda (x) (y x)) 'y) #f) 


