#lang scheme

(define (if-exp? exp) (eq? (first exp) 'if))
(define (lambda-exp? exp) (eq? (first exp) 'lambda))
(define (get-bindings exp) (cadr exp))
(define (get-exp exp) (caddr exp))

(define (find-pos-helper sym los)
(if (equal? sym (car los)) 0
          (if (find-pos sym (cdr los))
              (+ 1 (find-pos sym (cdr los)))
              #f)))
          
(define (find-pos sym los)
  (if (null? los) #f
      (find-pos-helper sym los)))

;; Failed attempt

;; Solution in a two pass approach
;; I thought of building up the binding list as
;; bindings: ({<binding>}*)
;; tree-of-bindings: (<bindings> | ({<bindings>}*))
;; So for, (lambda (a) (lambda (b) (a b))
;; this will build up:
;; ((a) ((b))
;; For an if expression, this will give:
;; (if (lambda (a) a) (lambda (b) b) (lambda (c) c))
;; the tree:
;; ((a) (b) (c))

;; While I thought this approach could work, I can see
;; that variable shadowing would become a problem here.
;; (lambda (x) (lambda (y) (x (lambda (x) x))))
;; In this expression, the x in the inner expression
;; is different from the x in the outermost expression.
;; The required result here is:
;; (lambda (x) (lambda (y) ((x : 1 0) (lambda (x) (x : 0 0)))
;; Now collecting the variables will give
;; ((x) ((y) ((x)))
;; Using this to acquire the depth will give
;; x to be 0 0 in both cases giving the expression.
;; (lambda (x) (lambda (y) ((x : 0 0) (lambda (x) (x : 0 0)))
;; There needs to be a particular context sensitivity which needs to
;; be present when doing the traversal.

;; One of the attempts to acquire the depth information by using
;; all the bindings of a lambda expression and then getting
;; finding the depth/position of the variable in the so formed tree.

;; Once the tree is formed, I conducted a search for finding the path
;; to the bound variable and then counting the number of contours.

;; This method failed because there needs to be a certain context marker
;; present inside each of the search to correctly locate the number of
;; contours crossed.

(define (get-sub-exp-bindings list-of-sub-exps)
  (filter (lambda (n) (not (empty? n))) (map all-bindings list-of-sub-exps)))

(define (all-bindings exp)
  (if (symbol? exp) '()
      (if (if-exp? exp) (get-sub-exp-bindings (list (cadr exp) (caddr exp) (cadddr exp)))
          (if (lambda-exp? exp)
              (if (empty? (all-bindings (get-exp exp)))
                  (list 'λ (get-bindings exp))
                  (list 'λ (get-bindings exp) (all-bindings (get-exp exp))))
              (get-sub-exp-bindings exp)))))

(all-bindings '(lambda (a) (lambda (b c) ((if (lambda (x) x) (lambda (y) y) (lambda (z) z)) (a (b c))))))
(all-bindings '(if (lambda (x) x) (lambda (y) y) (lambda (z) z)))

(define (find-last-occurrence-dp-in-list sym lob)
  (if (null? lob) #f
      (if (equal? (find-last-occurrence-dp sym (car lob)) #f)
          (find-last-occurrence-dp-in-list sym (cdr lob))
          (find-last-occurrence-dp sym (car lob)))))

(define (count-lambdas bindings)
  (if (null? bindings) 0
      (if (symbol? (car bindings))
                   (if (null? (cddr bindings)) 0
                       (if (equal? (car bindings) 'λ)
                           (+ 1 (count-lambdas (caddr bindings)))
                           (count-lambdas (caddr bindings))))
                   (count-lambdas (car bindings)))))

(define (find-last-occurrence-dp sym bindings)
  (if (null? bindings) #f
      (if (null? (car bindings)) #f
          (if (equal? (car bindings) 'λ)
              (if (find-pos sym (cadr bindings))
                  (if (find-last-occurrence-dp sym (cddr bindings))
                      (find-last-occurrence-dp sym (cddr bindings))
                      (list (count-lambdas bindings) (find-pos sym (cadr bindings))))
                  (find-last-occurrence-dp sym (cddr bindings)))
              (find-last-occurrence-dp-in-list sym bindings)))))

(equal? (find-last-occurrence-dp 'a '(λ (a))) '(0 0))
(equal? (find-last-occurrence-dp 'b '(λ (a b))) '(0 1))
(equal? (find-last-occurrence-dp 'c '(λ (a b c))) '(0 2))
(equal? (find-last-occurrence-dp 'c '(λ (a b c))) '(0 2))
(equal? (find-last-occurrence-dp 'a '(λ (a) (λ (b)))) '(1 0))
(equal? (find-last-occurrence-dp 'a '(λ (a) (λ (b) (λ (c))))) '(2 0))
(equal? (find-last-occurrence-dp 'a '(λ (x y a) (λ (b) (λ (c))))) '(2 2))
(equal? (find-last-occurrence-dp 'a '(λ (b) (λ (b a)))) '(0 1))
(equal? (find-last-occurrence-dp 'm '(λ (a) (λ (b) (λ (c) (λ (m) (λ (n) (λ (p)))))))) '(2 0))
(equal? (find-last-occurrence-dp 'x '(λ (a))) #f)
(equal? (find-last-occurrence-dp 'x '(λ (x) (λ (y) (λ (x))))) '(0 0))

;; Uses cadr of result as the free-var-count for the continuation
(define (build-app-helper fst-app exp bindings depth)
  (if (null? exp) '()
  (list (cons (car fst-app)
              (car (build-application (cdr exp) bindings (cadr fst-app) depth)))
        (cadr (build-application (cdr exp) bindings (cadr fst-app) depth)))))          

(define (build-application exp bindings free-var-count depth)
  (if (null? exp) (list '() free-var-count)
      (build-app-helper (la-helper (car exp) bindings free-var-count depth)
                        exp bindings depth)))

(define (assign-dp exp dp free-var-count env-depth)
  (if dp
  (list (append (list exp ':) dp) free-var-count)
  (list (append (list exp ':) (list env-depth free-var-count)) (+ free-var-count 1))))

(define (build-lambda-exp bindings result)
  (list (list 'lambda bindings (car result)) (cadr result)))

(define (build-if-application result)
  (list (cons 'if (car result)) (cadr result)))

; la-helper exp bindings store free-var-count depth
(define (la-helper exp bindings free-var-count env-depth)
  (if (symbol? exp)
      (assign-dp exp (find-last-occurrence-dp exp bindings) free-var-count env-depth)
      (if (if-exp? exp) (build-if-application (build-application (cdr exp) bindings free-var-count env-depth)) 
                                   (if (lambda-exp? exp)
                                       (build-lambda-exp (get-bindings exp) (la-helper (get-exp exp) bindings free-var-count (+ 1 env-depth)))
                                       (build-application exp bindings free-var-count env-depth)))))

(define (lexical-address exp)
  (car (la-helper exp (all-bindings exp) 0 0)))

(equal? (lexical-address '(lambda (a) a)) '(lambda (a) (a : 0 0)))
(equal? (lexical-address '(lambda (x y) y)) '(lambda (x y) (y : 0 1)))
(equal? (lexical-address '(lambda () m)) '(lambda () (m : 1 0)))
(equal? (lexical-address '(lambda (x) (m (lambda (y) n)))) '(lambda (x) ((m : 1 0) (lambda (y) (n : 2 1)))))
(equal? (lexical-address '(lambda (a b c) m)) '(lambda (a b c) (m : 1 0)))
(equal? (lexical-address '(x y)) '((x : 0 0) (y : 0 1)))
(equal? (lexical-address '(lambda (x y) (x y))) '(lambda (x y) ((x : 0 0) (y : 0 1))))
(equal? (lexical-address '(lambda (x) (lambda (y) y))) '(lambda (x) (lambda (y) (y : 0 0))))
(equal? (lexical-address '(lambda (x) (lambda (y) x))) '(lambda (x) (lambda (y) (x : 1 0))))
(equal? (lexical-address '(lambda (x y) (lambda (a) (a (x y))))) '(lambda (x y) (lambda (a) ((a : 0 0) ((x : 1 0) (y : 1 1))))))
(equal? (lexical-address '(lambda (x) (lambda (y) (m (a (lambda (z) n))))))
         '(lambda (x) (lambda (y) ((m : 2 0) ((a : 2 1) (lambda (z) (n : 3 2)))))))
(equal? (lexical-address '(if a x y)) '(if (a : 0 0) (x : 0 1) (y : 0 2)))
(equal? (lexical-address '(if (eq? a b) x y)) '(if ((eq? : 0 0) (a : 0 1) (b : 0 2)) (x : 0 3) (y : 0 4)))
(equal? (lexical-address '(lambda (x) (x x))) '(lambda (x) ((x : 0 0) (x : 0 0))))
(equal? (lexical-address '(lambda (a b c) (if (eq? b c) ((lambda (c) (cons a c)) a) b)))
     '(lambda (a b c) (if ((eq? : 1 0) (b : 0 1) (c : 0 2))
                          ((lambda (c) ((cons : 2 1) (a : 1 0) (c : 0 0))) (a : 0 0)) (b : 0 1))))

;; This is a good case to understand why it fails. The examples so far assumes that the innermost
;; lambda expression is the spot from which all previously bound variables will be references.
;; This restriction came in because of the find-last-occurrence-dp function which takes
;; all-bindings as its parameter. all-bindings is a static value that doesn't change through
;; the course of the evaluation of the program and hence always recourses to finding the depth
;; of variables from the innermost context. 
(equal? (lexical-address '(lambda (a) (a (lambda (y) a)))) '(lambda (a) ((a : 0 0) (lambda (y) (a : 1 0)))))

;; This works for functions for which the variables are referenced from the
;; last binding spot / innermost context but fails for all others, such as:
(equal? (lexical-address '(lambda (x y) ((lambda (a) (x (a y))) x))) '(lambda (x y) ((lambda (a) ((x : 0 0) ((a : 0 0) (y : 1 1)))) (x : 0 0))))

;; This could be fixed by progressively selecting the appropriate context that
;; applies to the expression being processed. This is the point at which
;; I stopped at this approach and chose the approach by which I solved it.

;; Previous attempt

(define (search-list sym lob)
  (if (find-pos sym (car lob))
      (list 0 (find-pos sym (car lob)))
      (find-dp sym (cdr lob))))

(define (increment-pos result)
  (if (and result (= (car result) 0)) (list (car result) (+ (cadr result) 1)) result))

;; lob: <sym> | ({<lob>}*)
(define (find-dp sym bindings)
  (if (null? bindings) #f
      (if (find-pos sym (car bindings))
          (list 0 (find-pos sym (car bindings)))
          (if (find-dp sym (cdr bindings))
              (list (+ 1 (car (find-dp sym (cdr bindings))))
                    (cadr (find-dp sym (cdr bindings))))
              #f))))
#|
(andmap (λ (n) (equal? n true)) (list (equal? (find-dp 'x '(x)) '(0 0))
(equal? (find-dp 'x '(())) #f)
(equal? (find-dp 'x '(x y)) '(0 0))
(equal? (find-dp 'y '(x y)) '(0 1))
(equal? (find-dp 'z '(x y z)) '(0 2))
(equal? (find-dp 'x '(y (x))) '(1 0))
(equal? (find-dp 'x '(a b c (x))) '(1 0))
(equal? (find-dp 'x '(a b (c d x))) '(1 2))
(equal? (find-dp 'x '(a b (c d (x)))) '(2 0))))
|#

;; Approach B

;; Uses cadr of result as the free-var-count for the continuation
(define (build-list-helper fst-app exp bindings depth)
  (if (null? exp) '()
      (list (cons (car fst-app)
                  (car (build-list (cdr exp) bindings (cadr fst-app) depth)))
            (cadr (build-list (cdr exp) bindings (cadr fst-app) depth)))))          

(define (build-list exp bindings free-var-count depth)
  (if (null? exp) (list '() free-var-count)
      (build-list-helper (la-helper-B (car exp) bindings free-var-count depth)
                         exp bindings depth)))

(define (assign-dp-B exp dp free-var-count env-depth)
  (if dp
      (list (append (list exp ':) dp) free-var-count)
      (list (append (list exp ':) (list env-depth free-var-count)) (+ free-var-count 1))))



(define (la-helper-B exp bindings free-var-count env-depth)
  (if (symbol? exp)
      (assign-dp-B exp (find-dp exp bindings) free-var-count env-depth)
      (if (if-exp? exp) (build-if-application (build-list (cdr exp) bindings free-var-count env-depth)) 
          (if (lambda-exp? exp)
              (build-lambda-exp (get-bindings exp) (la-helper-B (get-exp exp) (cons (get-bindings exp) bindings) free-var-count (+ 1 env-depth)))
              (build-list exp bindings free-var-count env-depth)))))

(define (lexical-address-B exp) (car (la-helper-B exp '() 0 0)))


; Simple expressions
(equal? (lexical-address-B '(lambda (a) a)) '(lambda (a) (a : 0 0)))
(equal? (lexical-address-B '(lambda (x y) y)) '(lambda (x y) (y : 0 1)))
(equal? (lexical-address-B '(x y)) '((x : 0 0) (y : 0 1)))
(equal? (lexical-address-B '(lambda (x y) (x y))) '(lambda (x y) ((x : 0 0) (y : 0 1))))
(equal? (lexical-address-B '(lambda (x) (lambda (y) y))) '(lambda (x) (lambda (y) (y : 0 0))))
(equal? (lexical-address-B '(lambda (x) (lambda (y) x))) '(lambda (x) (lambda (y) (x : 1 0))))
(equal? (lexical-address-B '(lambda (x) (x x))) '(lambda (x) ((x : 0 0) (x : 0 0))))

; Multiple application
(equal? (lexical-address-B '(lambda (x) (x (x x)))) '(lambda (x) ((x : 0 0) ((x : 0 0) (x : 0 0)))))
(equal? (lexical-address-B '(lambda (a b c d ) (a ((b c) d)))) '(lambda (a b c d) ((a : 0 0) (((b : 0 1) (c : 0 2)) (d : 0 3)))))
(equal? (lexical-address-B '(lambda (x y) ((lambda (a) (x (a y))) x))) '(lambda (x y) ((lambda (a) ((x : 1 0) ((a : 0 0) (y : 1 1)))) (x : 0 0))))
(equal? (lexical-address-B '(lambda (x y) (lambda (a) (a (x y))))) '(lambda (x y) (lambda (a) ((a : 0 0) ((x : 1 0) (y : 1 1))))))
(equal? (lexical-address-B '(lambda (a) (a (lambda (y) a)))) '(lambda (a) ((a : 0 0) (lambda (y) (a : 1 0)))))

; Unbound variables
(equal? (lexical-address-B '(lambda () m)) '(lambda () (m : 1 0)))
(equal? (lexical-address-B '(lambda (a b c) m)) '(lambda (a b c) (m : 1 0)))
(equal? (lexical-address-B '(lambda (x) (m (lambda (y) n)))) '(lambda (x) ((m : 1 0) (lambda (y) (n : 2 1)))))
(equal? (lexical-address-B '(lambda (x) (lambda (y) (m (a (lambda (z) n))))))
        '(lambda (x) (lambda (y) ((m : 2 0) ((a : 2 1) (lambda (z) (n : 3 2)))))))

; If Expressions
(equal? (lexical-address-B '(if a x y)) '(if (a : 0 0) (x : 0 1) (y : 0 2)))
(equal? (lexical-address-B '(if (eq? a b) x y)) '(if ((eq? : 0 0) (a : 0 1) (b : 0 2)) (x : 0 3) (y : 0 4)))

(equal? (lexical-address-B '(lambda (a b c) (if (eq? b c) ((lambda (c) (cons a c)) a) b)))
        '(lambda (a b c) (if ((eq? : 1 0) (b : 0 1) (c : 0 2))
                             ((lambda (c) ((cons : 2 1) (a : 1 0) (c : 0 0))) (a : 0 0)) (b : 0 1))))

(equal? (lexical-address-B '(lambda (b) (if ((lambda (b) c) b)
                                          (lambda (a) (a b)) (lambda (c) m))))
        '(lambda (b) (if ((lambda (b) (c : 2 0)) (b : 0 0))
                         (lambda (a) ((a : 0 0) (b : 1 0))) (lambda (c) (m : 2 1)))))

(equal? (lexical-address-B '(lambda (a b c) (cons a (cons b c)))) '(lambda (a b c) ((cons : 1 0) (a : 0 0)
                                                                                         ((cons : 1 0)
                                                                                          (b : 0 1) (c : 0 2)))))


  #|
  (define (add-bindings bindings binding-store)
  (if (empty? binding-store) bindings (cons binding-store (list bindings))))

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

(equal? (lexical-address 'a) '(a : 0 0))
(equal? (lexical-address '(lambda (a) a)) '(lambda (a) (a : 0 0)))
(equal? (lexical-address '(lambda (x y) y)) '(lambda (x y) (y : 0 1)))
(equal? (lexical-address '(lambda () m)) '(lambda () (m : 0 0)))
(equal? (lexical-address '(lambda (a b c) m)) '(lambda (a b c) (m : 0 0)))
;(equal? (lexical-address '(x y)) '((x : 0 0) (y : 0 1)))
(equal? (lexical-address '(lambda (x y) (x y))) '(lambda (x) '((x : 0 0) (y : 0 1))))
;(equal? (lexical-address '(lambda (x) (lambda (y) y))) '(lambda (x) (lambda (y) '(y : 1 0))))
(equal? (lexical-address '(lambda (x y) ((lambda (a) (x (a y))) x))) '(lambda (x y) ((lambda (a) ((x : 1 0) ((a: 0 0) (y: 1 1)))))))
|#