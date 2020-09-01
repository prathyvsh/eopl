#lang scheme
(require racket/trace)

;; Helpers
(define (if-exp? exp) (eq? (first exp) 'if))
(define (lambda-exp? exp) (eq? (first exp) 'lambda))
(define (get-bindings exp) (cadr exp))
(define (get-exp exp) (caddr exp))

(define (get-symbol sdp) (car sdp))
(define (get-depth sdp) (cdar sdp))
(define (get-length sdp) (cdadr sdp))

(define (find-pos-helper sym los)
(if (equal? sym (car los)) 0
          (if (find-pos sym (cdr los))
              (+ 1 (find-pos sym (cdr los)))
              #f)))
          
(define (find-pos sym los)
  (if (null? los) #f
      (find-pos-helper sym los)))

(define (increment-depth result)
  (if result (list (+ (car result) 1) (cadr result))
      result))

;; Approach 1
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

(define (get-sub-exp-bindings list-of-sub-exps)
  (filter (lambda (n) (not (empty? n))) (map all-bindings list-of-sub-exps)))

;; This is the function to get the list of all bindings
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


;; The path of bindings to the expression being processed needs
;; to be built up to get the depth/position of the symbol bound.
;; This can be done by progressively removing the outerbindings
;; as the traversal is performed with this approach.

(define (build-application exp bindings)
  (if (null? exp) '()
      (cons (lexical-address-helper (car exp) bindings)
            (build-application (cdr exp) bindings))))

(define (lexical-address-helper exp bindings)
  (if (symbol? exp) (append (list exp ':) (find-last-occurrence-dp exp bindings))
      (if (lambda-exp? exp) (list 'lambda (get-bindings exp) (lexical-address-helper (get-exp exp) bindings))
          (build-application exp bindings))))

(define (lexical-address exp)
  (lexical-address-helper exp (all-bindings exp)))

(equal? (lexical-address '(lambda (a) a)) '(lambda (a) (a : 0 0)))
(equal? (lexical-address '(lambda (x y) y)) '(lambda (x y) (y : 0 1)))
; (equal? (lexical-address '(lambda () m)) '(lambda () (m : 0 0)))
; (equal? (lexical-address '(lambda (a b c) m)) '(lambda (a b c) (m : 0 0)))
; (equal? (lexical-address '(x y)) '((x : 0 0) (y : 0 1)))
(equal? (lexical-address '(lambda (x y) (x y))) '(lambda (x y) ((x : 0 0) (y : 0 1))))
(equal? (lexical-address '(lambda (x) (lambda (y) y))) '(lambda (x) (lambda (y) (y : 0 0))))
; (equal? (lexical-address '(lambda (x y) ((lambda (a) (x (a y))) x))) '(lambda (x y) ((lambda (a) ((x : 1 0) ((a: 0 0) (y: 1 1)))))))
(equal? (lexical-address '(lambda (x y) (lambda (a) (a (x y))))) '(lambda (x y) (lambda (a) ((a : 0 0) ((x : 1 0) (y : 1 1))))))

(define (search-list sym lob)
  (if (find-pos sym (car lob))
      (list 0 (find-pos sym (car lob)))
      (find-dp sym (cdr lob))))


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