#lang scheme

;; Thoughts
;; In finding out the free variables of an expression, do you necessarily
;; need to know the bindings so far?
;; Not really, you only need to know the bindings of the immediate context to
;; determine if the free variables found so far are bound or not.
;; Proceeding in this manner in recursively removing the bound variables, one
;; can arrive at the list of free variables.
;; I can't help but think this is some kind of subtraction of indices happening,
;; if you think of these as De Bruijn indices.

;; My previous answer to this question was yes.
;; With my reasoning that the nested context in which an expression is embedded
;; determines whether a variable is free or not. Adding a lambda function
;; as a new context around a certain binding can have a change in meaning
;; for a variable even if it is deeply nested within other expressions.
;; For example in:
;; free-p = (λ (z) (λ (y) (λ (x) p)))
;; p is a free variable
;; But if we go to provide a context for it as
;; (λ (p) free-p), the p becomes bound to the formal parameter introduced.
;; By this reasoning, I thought this sensitivity to the context lead to
;; an algorithm that progressively keeps track of the bindings that have been
;; performed so far. This could be of use in certain other contexts, which I need
;; to characterize carefully, but to figure out whether a given variable is free or
;; not, one can proceed piecemeal by successively removing the bound variables through
;; a structural recursion.

;; I think, I again have to revise my answer now.
;; There is a case where a variable can be both bound or unbound ((lambda (x) x) x)
;; Here, the outer x is free while the inner x is bound.
;; Consider this carefully. I think this is where alpha renaming becomes significant.

(define (cons-without-duplicates s l)
  (if (contains? s l) l (cons s l)))

(define (merge-without-duplicates l1 l2)
  (if (null? l1) l2
      (merge-without-duplicates (cdr l1) (cons-without-duplicates (car l1) l2))))

(define (get-binding lambda-exp)
  (if (null? (cadr lambda-exp)) '() (caadr lambda-exp)))

(define (get-exp lambda-exp)
  (caddr lambda-exp))

(define (contains? sym los)
  (if (null? los) false
      (if (equal? (car los) sym) true
          (contains? sym (cdr los)))))

(define (remove sym los)
  (if (null? los) '()
      (if (equal? sym (car los)) (remove sym (cdr los))
      (cons (car los) (remove sym (cdr los))))))

(define (remove-bound-variable var fvs) (remove var fvs))

(define (get-bound-variable var fvs) (contains? var fvs))

;; Free variables in the lambda expression are the ones that are
;; remaining after the bound ones are removed.
(define (free-vars-in-lambda-exp lexp)
  (remove-bound-variable (get-binding lexp) (free-vars (get-exp lexp))))

(define (free-vars-in-application exp)
  (merge-without-duplicates (free-vars (car exp)) (free-vars (cadr exp))))

;; exp : <varref> | (lambda (<var>) <exp>) | (<exp> <exp>)
(define (free-vars exp)
  (if (symbol? exp) (list exp)
      (if (eq? (first exp) 'lambda) (free-vars-in-lambda-exp exp)
          (free-vars-in-application exp))))


(define (set-equal? x y)
  (if (equal? (length x) (length y))
      (if (and (empty? x) (empty? y)) #t
          (if (or (empty? x) (empty? y)) #f
              (andmap (lambda (n) (contains? n y)) x)))
      #f))

(set-equal? (free-vars 'a) '(a))
(set-equal? (free-vars '(lambda (a) a)) '())
(set-equal? (free-vars '(lambda (a) (a b))) '(b))
(set-equal? (free-vars '((lambda (a) a) b)) '(b))
(set-equal? (free-vars '(lambda () (x y))) '(x y))
(set-equal? (free-vars '(lambda (x) (lambda (y) (x y)))) '())
(set-equal? (free-vars '(lambda (x) (lambda (y) p))) '(p))
(set-equal? (free-vars '(lambda (x) ((lambda (y) y) x))) '())
(set-equal? (free-vars '(lambda (x1) ((lambda (y1) y) x))) '(y x))
(set-equal? (free-vars '(lambda (x) (lambda (y) (lambda (z) (x y z))))) '())
(set-equal? (free-vars '(lambda (x) (lambda (y) ((lambda (z) (p q)) r)))) '(q p r))
(set-equal? (free-vars '(lambda () (lambda () x))) '(x))