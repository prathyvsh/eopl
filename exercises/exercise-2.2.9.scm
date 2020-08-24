#lang scheme
(require racket/trace)

(define (path n bst)
  (if (null? bst) (error "Couldn't find the element in the binary search tree")
      (if (equal? n (car bst)) '()
      (if (< n (car bst)) (cons 'L (path n (cadr bst)))
          (cons 'R (path n (caddr bst)))))))

(equal? (path 1 '(1)) '())
(equal? (path 2 '(10 (5 (2 () ()))
                     (12 () ()))) '(L L))
(equal? (path 17 '(14 (7 () (12 () ()))
                      (26 (20 (17 () ())
                              ())
                          (31 () ())))) '(R L L))
(equal? (path 31 '(14 (7 () (12 () ()))
                      (26 (20 (17 () ())
                              ())
                          (31 () ())))) '(R R))

;; s-exp: symbol | s-list
;; s-list: ({<s-exp>}*)
;; There is a reversal of operations happening when seeking
;; for the match
(define (visit-pattern s slst errvalue)
  (if (null? slst) errvalue
      (if (symbol? slst)
          (if (eq? s slst) '() errvalue)
          (if (eq? (visit-pattern s (car slst) errvalue) errvalue)
              (if (eq? (visit-pattern s (cdr slst) errvalue) errvalue)
                  errvalue
                  (cons 'cdr (visit-pattern s (cdr slst) errvalue)))
                  (cons 'car (visit-pattern s (car slst) errvalue))))))

(define (reverse l)
  (if (null? l) '()
      (append (reverse (cdr l)) (list (car l)))))

(define (nest l)
  (if (null? l) '()
      (if (null? (cdr l)) (car l)
      (list (car l) (nest (cdr l))))))

(define (car&cdr-try1 s slst errvalue)
  (if (equal? (visit-pattern s slst errvalue) errvalue) errvalue
  (list 'lambda '(lst) (nest (append (reverse (visit-pattern s slst errvalue)) '(lst))))))

(equal? (car&cdr-try1 'a '(a b c) 'fail) '(lambda (lst) (car lst)))
(equal? (car&cdr-try1 'a '() 'fail) 'fail)
(equal? (car&cdr-try1 'a '(() () () a) 'fail) '(lambda (lst) (car (cdr (cdr (cdr lst))))))
(equal? (car&cdr-try1 'c '(a b c) 'fail) '(lambda (lst) (car (cdr (cdr lst)))))
(equal? (car&cdr-try1 'apple '((apple)) 'fail) '(lambda (lst) (car (car lst))))
(equal? (car&cdr-try1 'dog '(cat lion (fish dog) pig) 'fail) '(lambda (lst)
                                                           (car (cdr (car (cdr (cdr lst)))))))
(equal? (car&cdr-try1 'a '(b c) 'fail) 'fail)

;; First try while did give the correct answers, makes use of reverse and nets which needs to
;; to do O(n) traversals to get the correct ordering. I’m attempting to make the order correct
;; in a single pass with my next attempt.

(define (reverse-nest el l)
  (if (empty? l) (list 'lst)
      (if (empty? (cdr l))
          (if (list? (car l)) (reverse-nest el (car l))
              (list el (car l)))
      (list (car l) (reverse-nest el (cdr l))))))

(define (join-scans car-scan cdr-scan errvalue)
  (if (and (equal? car-scan errvalue) (equal? cdr-scan errvalue))
      errvalue
      (if (equal? car-scan errvalue)
          (reverse-nest 'cdr cdr-scan)
          (reverse-nest 'car car-scan))))

(define (scan-sexp s sexp errvalue)
  (if (symbol? sexp)
      (if (equal? s sexp)'() errvalue)
      (if (equal? (access-pattern s sexp errvalue) errvalue)
          errvalue
          (access-pattern s sexp errvalue))))

(define (access-pattern s slst errvalue)
  (if (null? slst) errvalue
          (join-scans (scan-sexp s (car slst) errvalue)
                      (scan-sexp s (cdr slst) errvalue) errvalue)))

(define (car&cdr s slst errvalue)
  (if (equal? (access-pattern s slst errvalue) errvalue) errvalue
      (list 'lambda '(lst) (list 'car (access-pattern s slst errvalue)))))

(equal? (car&cdr 'a '() 'fail) 'fail)

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (eval-test quoted-fn-producer key data)
   (let* ((fn (eval (quoted-fn-producer key data 'fail) ns)))
     (equal? (fn data) key)))

(eval-test car&cdr 'a '(() ((a))))
(eval-test car&cdr 'a '(() (a)))
(eval-test car&cdr 'a '(() (b) a))
(eval-test car&cdr 'a '(b c d () e (a)))
(eval-test car&cdr 'a '(() () a))

(time (car&cdr-try1 'a '(b c d e f g (h i j k l (m n o p q r s (t u (v w) (x y))) (z a))) 'fail))
(time (car&cdr 'a '(b c d e f g (h i j k l (m n o p q r s (t u (v w) (x y))) (z a))) 'fail))

;; This definition works by using the reverse-nest function but I think it is much better
;; to use tail recursion here as it naturally models it as (c (b (a))) instead of
;; (a (b (c))) which is the model of traversal for normal recursion in Racket.

(define (car&cdr-tr s slst errvalue)
  (if (equal? (access-pattern-tr s slst errvalue) errvalue) errvalue
      (list 'lambda '(lst) (list 'car (access-pattern-tr s slst errvalue)))))

(define (scan-sexp-tr s sexp errvalue)
  (if (symbol? sexp)
      (if (equal? s sexp)'() errvalue)
      (if (equal? (access-pattern-tr s sexp errvalue) errvalue)
          errvalue
          (access-pattern-tr s sexp errvalue))))

(define (access-pattern-tr s slst errvalue store)
  (if (null? slst) errvalue
          (if (equal? (scan-sexp-tr s (car slst) errvalue) errvalue)
                      (scan-sexp-tr s (cdr slst) errvalue store)
                      (scan-sexp-tr s (cdr slst) errvalue))))