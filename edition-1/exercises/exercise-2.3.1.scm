#lang scheme

;; Thoughts
;; In finding out the free variables of an expression, do you necessarily
;; need to know of all the bindings so far?
;; There are two approaches to this:
;; In the first approach, if you have all the bindings from the nested contexts
;; so far, you can give a final absolute answer.
;; 
;; With the relative approach, you only need to know the bindings of the immediate context to
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

;; Attempt 1
(define (contains? sym l)
  (if (null? l) false
      (if (equal? sym (car l)) #t
          (contains? sym (cdr l)))))

(define (cons-without-duplicates s l)
  (if (contains? s l) l (cons s l)))

(define (merge-without-duplicates l1 l2)
  (if (null? l1) l2
      (merge-without-duplicates (cdr l1) (cons-without-duplicates (car l1) l2))))

(define (get-binding lambda-exp) (if (null? (cadr lambda-exp)) '() (caadr lambda-exp)))

(define (get-exp lambda-exp) (caddr lambda-exp))

(define (retain sym los)
  (if (null? los) '()
      (if (equal? (car los) sym) (cons (car los) (retain sym (cdr los)))
          (retain sym (cdr los)))))

(define (remove sym los)
  (if (null? los) '()
      (if (equal? sym (car los)) (remove sym (cdr los))
      (cons (car los) (remove sym (cdr los))))))

(define (remove-bound-variable var fvs) (remove var fvs))

;; Free variables in the lambda expression are the ones that are
;; remaining after the bound ones are removed.
(define (free-vars-in-lambda-exp lexp)
  (remove-bound-variable (get-binding lexp) (free-vars (get-exp lexp))))

(define (bound-vars-in-lambda-exp lexp)
  (merge-without-duplicates (retain (get-binding lexp) (free-vars (get-exp lexp)))
        (bound-vars (get-exp lexp))))

(define (free-vars-in-application exp)
  (merge-without-duplicates (free-vars (car exp)) (free-vars (cadr exp))))

(define (bound-vars-in-application exp)
  (merge-without-duplicates (bound-vars (car exp)) (bound-vars (cadr exp))))

;; exp : <varref> | (lambda (<var>) <exp>) | (<exp> <exp>)
(define (free-vars exp)
  (if (symbol? exp) (list exp)
      (if (eq? (car exp) 'lambda) (free-vars-in-lambda-exp exp)
          (free-vars-in-application exp))))

(define (bound-vars exp)
  (if (symbol? exp) '()
      (if (eq? (car exp) 'lambda) (bound-vars-in-lambda-exp exp)
          (bound-vars-in-application exp))))

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
(set-equal? (free-vars '(lambda (x) (lambda (y) ((lambda (z) (p q)) r)))) '(p q r))
(set-equal? (free-vars '(lambda () (lambda () x))) '(x))
(set-equal? (free-vars '(lambda (x) ((lambda (y) ((lambda (z) p) q)) r))) '(p q r))
(set-equal? (free-vars '(x (y (x z)))) '(x y z))

(set-equal? (bound-vars 'a) '())
(set-equal? (bound-vars '(lambda (a) a)) '(a))
(set-equal? (bound-vars '(a b)) '())
(set-equal? (bound-vars '(lambda (a) (a b))) '(a))
(set-equal? (bound-vars '(lambda () (x y))) '())
(set-equal? (bound-vars '(lambda (a) ((lambda (b) b) a))) '(a b))
(set-equal? (bound-vars '(x (y (x z)))) '())
(set-equal? (bound-vars '(lambda (x y) (lambda (z) (((lambda (y) y) y) ((lambda (z) x) (lambda (w) w)))))) '(x y w))

;; Attempt 2
;; exp : <varref> | (lambda (<var>) <exp>) | (<exp> <exp>)

;; An idea I just got is that you are doing a structural recursion on the same
;; set of expressions inorder to find if a variable is bound/free. This means
;; that both the expressions can have the same algorithmic trace.

(define (merge fb1 fb2)
  (list (merge-without-duplicates (car fb1) (car fb2))
        (merge-without-duplicates (cadr fb1) (cadr fb2))))

(define (add-binding binding result)
  (if (contains? binding (cadr result))
            (list (cons binding (car result))
            (remove binding (cadr result)))
            (list (car result) (cadr result))))

(define (abstraction? exp) (eq? (car exp) 'lambda))

;; Trace walks through the expression and finds the free and bound occurrences.
;; Whenever it finds a binding that is present in the variables list, it is subtracted
;; from the list and added to the list of bindings.
;; Finally merge-without-duplicates is used to merge the variables occurring as exp1 and exp2
;; in an application.
(define (trace exp)
  (if (symbol? exp) (list '() (list exp))
      (if (abstraction? exp) (add-binding (get-binding exp) (trace (get-exp exp)))
          (merge (trace (car exp)) (trace (cadr exp))))))

(define (free-vars2 exp) (cadr (trace exp)))

(define (bound-vars2 exp) (car (trace exp)))

(set-equal? (free-vars2 'a) '(a))
(set-equal? (free-vars2 '(lambda (a) a)) '())
(set-equal? (free-vars2 '(lambda (a) (a b))) '(b))
(set-equal? (free-vars2 '((lambda (a) a) b)) '(b))
(set-equal? (free-vars2 '(lambda () (x y))) '(x y))
(set-equal? (free-vars2 '(lambda (x) (lambda (y) (x y)))) '())
(set-equal? (free-vars2 '(lambda (x) (lambda (y) p))) '(p))
(set-equal? (free-vars2 '(lambda (x) ((lambda (y) y) x))) '())
(set-equal? (free-vars2 '(lambda (x1) ((lambda (y1) y) x))) '(y x))
(set-equal? (free-vars2 '(lambda (x) (lambda (y) (lambda (z) (x y z))))) '())
(set-equal? (free-vars2 '(lambda (x) (lambda (y) ((lambda (z) (p q)) r)))) '(p q r))
(set-equal? (free-vars2 '(lambda () (lambda () x))) '(x))
(set-equal? (free-vars2 '(lambda (x) ((lambda (y) ((lambda (z) p) q)) r))) '(p q r))
(set-equal? (free-vars2 '(x (y (x z)))) '(x y z))

(set-equal? (bound-vars2 'a) '())
(set-equal? (bound-vars2 '(lambda (a) a)) '(a))
(set-equal? (bound-vars2 '(a b)) '())
(set-equal? (bound-vars2 '(lambda (a) (a b))) '(a))
(set-equal? (bound-vars2 '(lambda () (x y))) '())
(set-equal? (bound-vars2 '(lambda (a) ((lambda (b) b) a))) '(a b))
(set-equal? (bound-vars2 '(x (y (x z)))) '())

;; Attempt 3

(define (free-or-bound sym free-vars bound-vars bindings)
  (if (contains? sym bindings)
      (list free-vars (cons sym bound-vars))
      (list (cons sym free-vars) bound-vars)))
      
(define (process-lambda-fn lexp free-vars bound-vars bindings)
  (vars-tr-helper (get-exp lexp) free-vars bound-vars (cons (get-binding lexp) bindings)))

(define (process-application exp1 exp2 free-vars bound-vars bindings)
  (merge (vars-tr-helper exp1 free-vars bound-vars bindings) (vars-tr-helper exp2 free-vars bound-vars bindings)))
  
;; exp : <varref> | (lambda (<var>) <exp>) | (<exp> <exp>)
;; Return (list free-vars bound-vars)
(define (vars-tr-helper exp free-vars bound-vars bindings)
  (if (symbol? exp) (free-or-bound exp free-vars bound-vars bindings)
      (if (equal? (car exp) 'lambda) (process-lambda-fn exp free-vars bound-vars bindings)
         (process-application (car exp) (cadr exp) free-vars bound-vars bindings))))

(define (free-vars-tr exp) (car (vars-tr-helper exp '() '() '())))
(define (bound-vars-tr exp) (cadr (vars-tr-helper exp '() '() '())))

(set-equal? (free-vars-tr '(lambda (x) x)) '())
(set-equal? (free-vars-tr '(lambda () x)) '(x))
(set-equal? (free-vars-tr '(lambda (y) x)) '(x))
(set-equal? (free-vars-tr '(lambda (y) y)) '())
(set-equal? (free-vars-tr '(lambda (x) (lambda (y) x))) '())
(set-equal? (free-vars-tr '(lambda (x) (lambda (y) (x y)))) '())
(set-equal? (free-vars-tr '(lambda (x) (lambda (y) y))) '())
(set-equal? (free-vars-tr '((lambda (q) m) (lambda (m) q))) '(m q))
(set-equal? (free-vars-tr '(lambda (m) (lambda (n) ((n m) p)))) '(p))
(set-equal? (free-vars-tr '(lambda (m) (lambda (n) ((n m) ((lambda () m) (lambda () n)))))) '())
(set-equal? (bound-vars '(lambda (x) (x (lambda (y) (p y))))) '(x y))
(set-equal? (bound-vars '(lambda () (lambda (x) (lambda (y) (m x))))) '(x))
(set-equal? (bound-vars '(lambda (m) (lambda (y) (lambda (p) (m y))))) '(m y))
(set-equal? (bound-vars '(lambda () (x y))) '())