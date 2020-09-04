#lang scheme
(require racket/trace)

;; Helpers
(define (if-exp? exp) (eq? (first exp) 'if))
(define (lambda-exp? exp) (eq? (first exp) 'lambda))
(define (get-bindings exp) (cadr exp))
(define (get-exp exp) (caddr exp))

#|
(define (get-symbol sdp) (car sdp))
(define (get-depth sdp) (cdar sdp))
(define (get-length sdp) (cdadr sdp))
|#

(define (find-pos-helper sym los)
(if (equal? sym (car los)) 0
          (if (find-pos sym (cdr los))
              (+ 1 (find-pos sym (cdr los)))
              #f)))
          
(define (find-pos sym los)
  (if (null? los) #f
      (find-pos-helper sym los)))

; (define (increment-depth result)
;   (if result (list (+ (car result) 1) (cadr result))
;       result))

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

(define (find-dp sym bindings)
  (if (null? bindings) #f
          (if (find-pos sym (car bindings))
              (list 0 (find-pos sym (car bindings)))
              (if (find-dp sym (cdr bindings))
                  (list (+ 1 (car (find-dp sym (cdr bindings))))
                    (cadr (find-dp sym (cdr bindings))))
                  #f))))
                

(equal? (find-dp 'x '((x))) (list 0 0))
(equal? (find-dp 'x '((y) (x))) (list 1 0))
(equal? (find-dp 'x '((y) (x) (m n))) (list 1 0))


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
      (assign-dp exp (find-dp exp bindings) free-var-count env-depth)
      (if (if-exp? exp) (build-if-application (build-application (cdr exp) bindings free-var-count env-depth)) 
                                   (if (lambda-exp? exp)
                                       (build-lambda-exp (get-bindings exp) (la-helper (get-exp exp) (cons (get-bindings exp) bindings) free-var-count (+ 1 env-depth)))
                                       (build-application exp bindings free-var-count env-depth)))))

(define (lexical-address exp) (car (la-helper exp '() 0 0)))

(equal? (lexical-address '(lambda (a) a)) '(lambda (a) (a : 0 0)))
(equal? (lexical-address '(lambda (x y) y)) '(lambda (x y) (y : 0 1)))
(equal? (lexical-address '(lambda () m)) '(lambda () (m : 1 0)))
(equal? (lexical-address '(lambda (x) (m (lambda (y) n)))) '(lambda (x) ((m : 1 0) (lambda (y) (n : 2 1)))))
(equal? (lexical-address '(lambda (a b c) m)) '(lambda (a b c) (m : 1 0)))
(equal? (lexical-address '(x y)) '((x : 0 0) (y : 0 1)))
(equal? (lexical-address '(lambda (x y) (x y))) '(lambda (x y) ((x : 0 0) (y : 0 1))))
(equal? (lexical-address '(lambda (x) (lambda (y) y))) '(lambda (x) (lambda (y) (y : 0 0))))
(equal? (lexical-address '(lambda (x) (lambda (y) x))) '(lambda (x) (lambda (y) (x : 1 0))))
(equal? (lexical-address '(lambda (x y) ((lambda (a) (x (a y))) x))) '(lambda (x y) ((lambda (a) ((x : 1 0) ((a : 0 0) (y : 1 1)))) (x : 0 0))))
(equal? (lexical-address '(lambda (x y) (lambda (a) (a (x y))))) '(lambda (x y) (lambda (a) ((a : 0 0) ((x : 1 0) (y : 1 1))))))
(equal? (lexical-address '(lambda (x) (lambda (y) (m (a (lambda (z) n))))))
        '(lambda (x) (lambda (y) ((m : 2 0) ((a : 2 1) (lambda (z) (n : 3 2)))))))
(equal? (lexical-address '(if a x y)) '(if (a : 0 0) (x : 0 1) (y : 0 2)))
(equal? (lexical-address '(if (eq? a b) x y)) '(if ((eq? : 0 0) (a : 0 1) (b : 0 2)) (x : 0 3) (y : 0 4)))
(equal? (lexical-address '(lambda (x) (x x))) '(lambda (x) ((x : 0 0) (x : 0 0))))
(equal? (lexical-address '(lambda (a) (a (lambda (y) a)))) '(lambda (a) ((a : 0 0) (lambda (y) (a : 1 0)))))
(equal? (lexical-address '(lambda (a b c) (if (eq? b c) ((lambda (c) (cons a c)) a) b)))
     '(lambda (a b c) (if ((eq? : 1 0) (b : 0 1) (c : 0 2))
                          ((lambda (c) ((cons : 2 1) (a : 1 0) (c : 0 0))) (a : 0 0)) (b : 0 1))))
(equal? (lexical-address '(lambda (b) (if ((lambda (b) c) b)
                                          (lambda (a) (a b)) (lambda (c) m))))
        '(lambda (b) (if ((lambda (b) (c : 2 0)) (b : 0 0))
                         (lambda (a) ((a : 0 0) (b : 1 0))) (lambda (c) (m : 2 1)))))
