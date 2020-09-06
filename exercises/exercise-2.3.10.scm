#lang scheme

;; Helpers
(define (if-exp? exp) (eq? (first exp) 'if))
(define (lambda-exp? exp) (eq? (first exp) 'lambda))
(define (get-bindings exp) (cadr exp))
(define (get-exp exp) (caddr exp))

(define (find-pos-helper sym los pos)
  (if (null? los) #f
      (if (equal? sym (car los)) pos
          (find-pos-helper sym (cdr los) (+ 1 pos)))))

(define (find-pos sym los) (find-pos-helper sym los 0))

(define (find-dp sym bindings)
  (if (null? bindings) #f
      (if (find-pos sym (car bindings))
          (list 0 (find-pos sym (car bindings)))
          (if (find-dp sym (cdr bindings))
              (list (+ 1 (car (find-dp sym (cdr bindings))))
                    (cadr (find-dp sym (cdr bindings))))
              #f))))

(define (contains? sym los)
  (if (null? los) #f
      (if (eq? sym (car los)) #t
          (contains? sym (cdr los)))))

(define (cons-without-duplicates s l)
  (if (contains? s l) l (cons s l)))

(define (merge-without-duplicates l1 l2)
  (if (null? l1) l2
      (merge-without-duplicates (cdr l1) (cons-without-duplicates (car l1) l2))))

; (equal? (find-dp 'x '((x))) (list 0 0))
; (equal? (find-dp 'x '((y) (x))) (list 1 0))
; (equal? (find-dp 'x '((y) (x) (m n))) (list 1 0))

;; Review on the work
;; After a few failed attempts, I was able to solve this
;; using Approach B. But this could be a bit hard to parse
;; without knowing the exact details that goes into its construction.
;; Now I introduce a two pass approach which
;; shows the issues involved more clearly here.

;; Note: By thinking about separating the concerns I got a new approach (C),
;; which unearthed an error in Approach B: That is free-variables were
;; always assigned a new index instead of reusing it.
;; I introduce this as Approach B.1 with the correction. 

;; Approach A

;; This is a two pass approach.
;; I first assign depth/position to all the bound
;; variables. This is done by building a list of
;; bound variables and their depth and position
;; in this list is acquired using find-dp function.
;; This function works by the virtue of the nested
;; binding structure formed.
;; This functions also marks free variables as ('sym 'free <exp-name>)
;; This is done in order to be used in the next stage.

(define (build-sym-list exp dp)
  (append (list 'sym exp ':) dp))

(define (build-list-A exp bindings)
  (map (lambda (exp) (index-bindings exp bindings)) exp))

(define (index-bindings exp bindings)
  (if (symbol? exp) (if (find-dp exp bindings) (build-sym-list exp (find-dp exp bindings)) (list 'sym 'free exp))
      (if (if-exp? exp) (cons 'if (build-list-A (cdr exp) bindings)) 
          (if (lambda-exp? exp)
              (list 'lambda (get-bindings exp) (index-bindings (get-exp exp) (cons (get-bindings exp) bindings)))
              (build-list-A exp bindings)))))

;; In this stage, the free variables are assigned
;; the depth and position by keeping track of
;; the contours crossed and acquiring the position
;; from a list of assignments.
;; Free variables that have been assigned an index once
;; is added to a list of assignments to assign a consistent
;; position index.

(define (free-var? exp) (equal? (cadr exp) 'free))

(define (process-results car-result cdr-result assignments)
  (list (cons (car car-result)
              (car cdr-result))
        (merge-without-duplicates (merge-without-duplicates assignments (cadr car-result)) (cadr cdr-result))))

(define (process-app-list exp assignments depth)
  (if (null? exp) (list '() assignments)
      (process-results (index-free-vars (car exp) assignments depth)
      (process-app-list (cdr exp) (cadr (index-free-vars (car exp) assignments depth)) depth) assignments)))

(define (get-sym exp) (caddr exp))

(define (index-free-vars exp assignments depth)
  (if (eq? (car exp) 'sym) (if (free-var? exp)
                               (if (contains? (get-sym exp) assignments)
                                   (list (list (get-sym exp) ': depth (find-pos (get-sym exp) assignments)) assignments)
                                   (list (list (get-sym exp) ': depth (length assignments)) (append assignments (list (get-sym exp)))))
                               (list (cdr exp) assignments))
      (if (if-exp? exp) (list (cons 'if (car (process-app-list (cdr exp) assignments depth))) (append
                                                                                               assignments
                                                                                               (cadr (process-app-list (cdr exp) assignments depth))))
          (if (lambda-exp? exp)
              (list (list 'lambda (get-bindings exp) (car (index-free-vars (get-exp exp) assignments (+ 1 depth))))
                    (merge-without-duplicates assignments (cadr (index-free-vars (get-exp exp) assignments (+ 1 depth)))))
              (process-app-list exp assignments depth)))))


(define (lexical-address-A exp)
  (car (index-free-vars (index-bindings exp '()) '() 0)))

(equal? (lexical-address-A '(lambda (a) a)) '(lambda (a) (a : 0 0)))
(equal? (lexical-address-A '(lambda (a) b)) '(lambda (a) (b : 1 0)))
(equal? (lexical-address-A '(a b)) '((a : 0 0) (b : 0 1)))
(equal? (lexical-address-A '(a (b b))) '((a : 0 0) ((b : 0 1) (b : 0 1))))
(equal? (lexical-address-A '(cons a (cons b c))) '((cons : 0 0) (a : 0 1) ((cons : 0 0) (b : 0 2) (c : 0 3))))
(equal? (lexical-address-A '(lambda (a b c) (cons a (cons b c)))) '(lambda (a b c) ((cons : 1 0) (a : 0 0) ((cons : 1 0) (b : 0 1) (c : 0 2)))))
(equal? (lexical-address-A '(lambda (a) a)) '(lambda (a) (a : 0 0)))
(equal? (lexical-address-A '(lambda (x y) y)) '(lambda (x y) (y : 0 1)))
(equal? (lexical-address-A '(x y)) '((x : 0 0) (y : 0 1)))
(equal? (lexical-address-A '(lambda (x y) (x y))) '(lambda (x y) ((x : 0 0) (y : 0 1))))
(equal? (lexical-address-A '(lambda (x) (lambda (y) y))) '(lambda (x) (lambda (y) (y : 0 0))))
(equal? (lexical-address-A '(lambda (x) (lambda (y) x))) '(lambda (x) (lambda (y) (x : 1 0))))
(equal? (lexical-address-A '(lambda (x) (x x))) '(lambda (x) ((x : 0 0) (x : 0 0))))
(equal? (lexical-address-A '(lambda (x) (x (x x)))) '(lambda (x) ((x : 0 0) ((x : 0 0) (x : 0 0)))))
(equal? (lexical-address-A '(lambda (a b c d ) (a ((b c) d)))) '(lambda (a b c d) ((a : 0 0) (((b : 0 1) (c : 0 2)) (d : 0 3)))))
(equal? (lexical-address-A '(lambda (x y) ((lambda (a) (x (a y))) x))) '(lambda (x y) ((lambda (a) ((x : 1 0) ((a : 0 0) (y : 1 1)))) (x : 0 0))))
(equal? (lexical-address-A '(lambda (x y) (lambda (a) (a (x y))))) '(lambda (x y) (lambda (a) ((a : 0 0) ((x : 1 0) (y : 1 1))))))
(equal? (lexical-address-A '(lambda (a) (a (lambda (y) a)))) '(lambda (a) ((a : 0 0) (lambda (y) (a : 1 0)))))
(equal? (lexical-address-A '(lambda () m)) '(lambda () (m : 1 0)))
(equal? (lexical-address-A '(lambda (a b c) m)) '(lambda (a b c) (m : 1 0)))
(equal? (lexical-address-A '(lambda (x) (m (lambda (y) n)))) '(lambda (x) ((m : 1 0) (lambda (y) (n : 2 1)))))
(equal? (lexical-address-A '(lambda (x) (lambda (y) (m (a (lambda (z) n))))))
        '(lambda (x) (lambda (y) ((m : 2 0) ((a : 2 1) (lambda (z) (n : 3 2)))))))
 (equal? (lexical-address-A '(if a x y)) '(if (a : 0 0) (x : 0 1) (y : 0 2)))
(equal? (lexical-address-A '(if (eq? a b) x y)) '(if ((eq? : 0 0) (a : 0 1) (b : 0 2)) (x : 0 3) (y : 0 4)))
(equal? (lexical-address-A '(lambda (a b c) (if (eq? b c) ((lambda (c) (cons a c)) a) b)))
        '(lambda (a b c) (if ((eq? : 1 0) (b : 0 1) (c : 0 2))
                             ((lambda (c) ((cons : 2 1) (a : 1 0) (c : 0 0))) (a : 0 0)) (b : 0 1))))
(equal? (lexical-address-A '(lambda (b) (if ((lambda (b) c) b)
                                          (lambda (a) (a b)) (lambda (c) m))))
        '(lambda (b) (if ((lambda (b) (c : 2 0)) (b : 0 0))
                         (lambda (a) ((a : 0 0) (b : 1 0))) (lambda (c) (m : 2 1)))))
(equal? (lexical-address-A '(lambda (a b c) (cons a (cons b c)))) '(lambda (a b c) ((cons : 1 0) (a : 0 0)
                                                                                        ((cons : 1 0)
                                                                                          (b : 0 1) (c : 0 2)))))

(define (build-lambda-exp bindings result)
  (list (list 'lambda bindings (car result)) (cadr result)))

(define (build-if-application result)
  (list (cons 'if (car result)) (cadr result)))

;; Uses cadr of result as the free-var-count for the continuation
(define (build-list-helper fst-app exp bindings depth)
  (if (null? exp) '()
      (list (cons (car fst-app)
                  (car (build-list (cdr exp) bindings (cadr fst-app) depth)))
            (cadr (build-list (cdr exp) bindings (cadr fst-app) depth)))))          

(define (build-list exp bindings free-var-count depth)
  (if (null? exp) (list '() free-var-count)
      (build-list-helper (la-helper (car exp) bindings free-var-count depth)
                         exp bindings depth)))

(define (assign-dp exp dp free-var-count env-depth)
  (if dp
      (list (append (list exp ':) dp) free-var-count)
      (list (append (list exp ':) (list env-depth free-var-count)) (+ free-var-count 1))))



(define (la-helper exp bindings free-var-count env-depth)
  (if (symbol? exp)
      (assign-dp exp (find-dp exp bindings) free-var-count env-depth)
      (if (if-exp? exp) (build-if-application (build-list (cdr exp) bindings free-var-count env-depth)) 
          (if (lambda-exp? exp)
              (build-lambda-exp (get-bindings exp) (la-helper (get-exp exp) (cons (get-bindings exp) bindings) free-var-count (+ 1 env-depth)))
              (build-list exp bindings free-var-count env-depth)))))

(define (lexical-address exp) (car (la-helper exp '() 0 0)))

#|
; Simple expressions
(equal? (lexical-address '(lambda (a) a)) '(lambda (a) (a : 0 0)))
(equal? (lexical-address '(lambda (x y) y)) '(lambda (x y) (y : 0 1)))
(equal? (lexical-address '(x y)) '((x : 0 0) (y : 0 1)))
(equal? (lexical-address '(lambda (x y) (x y))) '(lambda (x y) ((x : 0 0) (y : 0 1))))
(equal? (lexical-address '(lambda (x) (lambda (y) y))) '(lambda (x) (lambda (y) (y : 0 0))))
(equal? (lexical-address '(lambda (x) (lambda (y) x))) '(lambda (x) (lambda (y) (x : 1 0))))
(equal? (lexical-address '(lambda (x) (x x))) '(lambda (x) ((x : 0 0) (x : 0 0))))

; Multiple application
(equal? (lexical-address '(lambda (x) (x (x x)))) '(lambda (x) ((x : 0 0) ((x : 0 0) (x : 0 0)))))
(equal? (lexical-address '(lambda (a b c d ) (a ((b c) d)))) '(lambda (a b c d) ((a : 0 0) (((b : 0 1) (c : 0 2)) (d : 0 3)))))
(equal? (lexical-address '(lambda (x y) ((lambda (a) (x (a y))) x))) '(lambda (x y) ((lambda (a) ((x : 1 0) ((a : 0 0) (y : 1 1)))) (x : 0 0))))
(equal? (lexical-address '(lambda (x y) (lambda (a) (a (x y))))) '(lambda (x y) (lambda (a) ((a : 0 0) ((x : 1 0) (y : 1 1))))))
(equal? (lexical-address '(lambda (a) (a (lambda (y) a)))) '(lambda (a) ((a : 0 0) (lambda (y) (a : 1 0)))))

; Unbound variables
(equal? (lexical-address '(lambda () m)) '(lambda () (m : 1 0)))
(equal? (lexical-address '(lambda (a b c) m)) '(lambda (a b c) (m : 1 0)))
(equal? (lexical-address '(lambda (x) (m (lambda (y) n)))) '(lambda (x) ((m : 1 0) (lambda (y) (n : 2 1)))))
(equal? (lexical-address '(lambda (x) (lambda (y) (m (a (lambda (z) n))))))
        '(lambda (x) (lambda (y) ((m : 2 0) ((a : 2 1) (lambda (z) (n : 3 2)))))))

; If Expressions
(equal? (lexical-address '(if a x y)) '(if (a : 0 0) (x : 0 1) (y : 0 2)))
(equal? (lexical-address '(if (eq? a b) x y)) '(if ((eq? : 0 0) (a : 0 1) (b : 0 2)) (x : 0 3) (y : 0 4)))

(equal? (lexical-address '(lambda (a b c) (if (eq? b c) ((lambda (c) (cons a c)) a) b)))
        '(lambda (a b c) (if ((eq? : 1 0) (b : 0 1) (c : 0 2))
                             ((lambda (c) ((cons : 2 1) (a : 1 0) (c : 0 0))) (a : 0 0)) (b : 0 1))))

(equal? (lexical-address '(lambda (b) (if ((lambda (b) c) b)
                                          (lambda (a) (a b)) (lambda (c) m))))
        '(lambda (b) (if ((lambda (b) (c : 2 0)) (b : 0 0))
                         (lambda (a) ((a : 0 0) (b : 1 0))) (lambda (c) (m : 2 1)))))

(equal? (lexical-address '(lambda (a b c) (cons a (cons b c)))) '(lambda (a b c) ((cons : 1 0) (a : 0 0)
                                                                                         ((cons : 1 0)
                                                                                          (b : 0 1) (c : 0 2)))))

|#

;; Approach C
;; The idea is to perform the search and whenever there is a free variable encountered
;; it is added to a top-level store and the count of the store is used for indexing it.
;; This avoids the need to keep track of an integer to count the number of free variables
;; served and it falls out as the length of the store.