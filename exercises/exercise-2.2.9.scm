#lang scheme

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

(define (car&cdr s slst errvalue)
  (if (equal? (visit-pattern s slst errvalue) errvalue) errvalue
  (list 'lambda '(lst) (nest (append (reverse (visit-pattern s slst errvalue)) '(lst))))))

(equal? (car&cdr 'a '(a b c) 'fail) '(lambda (lst) (car lst)))
(equal? (car&cdr 'a '() 'fail) 'fail)
(equal? (car&cdr 'a '(() () () a) 'fail) '(lambda (lst) (car (cdr (cdr (cdr lst))))))
(equal? (car&cdr 'c '(a b c) 'fail) '(lambda (lst) (car (cdr (cdr lst)))))
(equal? (car&cdr 'apple '((apple)) 'fail) '(lambda (lst) (car (car lst))))
(equal? (car&cdr 'dog '(cat lion (fish dog) pig) 'fail) '(lambda (lst)
                                                           (car (cdr (car (cdr (cdr lst)))))))
(equal? (car&cdr 'a '(b c) 'fail) 'fail)

;; First try while did give the correct answers, makes use of reverse and nets which needs to
;; to do O(n) traversals to get the correct ordering. I’m attempting to make the order correct
;; in a single pass with my next attempt.

(define (reverse-nest el l)
  (if (empty? l) (list el)
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
      (if (equal? s sexp)'(lst) errvalue)
      (let ((result (access-pattern s sexp errvalue)))
      (if (equal? result errvalue) errvalue result))))

(define (access-pattern s slst errvalue)
  (if (null? slst) errvalue
          (join-scans (scan-sexp s (car slst) errvalue)
                      (scan-sexp s (cdr slst) errvalue) errvalue)))

(define (car&cdr-try1 s slst errvalue)
  (let ((result (access-pattern s slst errvalue)))
  (if (equal? result errvalue) errvalue
      (list 'lambda '(lst) result))))

(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))

(define (eval-test quoted-fn-producer key data)
   (let* ((fn (eval (quoted-fn-producer key data 'fail) ns)))
     (equal? (fn data) key)))

(equal? (car&cdr-try1 'a '() 'fail) 'fail)
(eval-test car&cdr-try1 'a '(a))
(eval-test car&cdr-try1 'a '(c b a))
(eval-test car&cdr-try1 'a '(() ((a))))
(eval-test car&cdr-try1 'a '(() (a)))
(eval-test car&cdr-try1 'a '(() (b) a))
(eval-test car&cdr-try1 'a '(b c d () e (a)))
(eval-test car&cdr-try1 'a '(() () a))

;; This definition works by using the reverse-nest function but I think it is much better
;; to use tail recursion here as it naturally models it as (c (b (a))) instead of
;; (a (b (c))) which is the model of traversal for normal recursion in Racket.

(define (get-result test errvalue)
  (if (equal? (cadr test) errvalue)
      errvalue
      (list 'lambda '(lst) (caar test))))
         
(define (car&cdr-tr s slst errvalue)
  (get-result (access-pattern-tr s slst errvalue (list '(lst) errvalue)) errvalue))

(define (scan-sexp-tr s sexp errvalue store)
  (if (symbol? sexp)
      (if (equal? s sexp) (access-pattern-tr s '() errvalue  (list (first store) 'success)) (list (first store) errvalue))
      (let ((result (access-pattern-tr s sexp errvalue store)))
      (if (equal? (cadr result) errvalue)
          (list (first store) errvalue)
          result))))

(define (access-pattern-tr s slst errvalue store)
  (if (null? slst) store
      (let ((result (scan-sexp-tr s (car slst) errvalue (list (list (cons 'car (first store))) (second store)))))
          (if (equal? (cadr result) errvalue)
                      (scan-sexp-tr s (cdr slst) errvalue (list (list (cons 'cdr (first store))) (second store)))
                      result))))

(eval-test car&cdr-tr 'a '(() ((a))))
(eval-test car&cdr-tr 'a '(() (a)))
(eval-test car&cdr-tr 'a '(() (b) a))
(eval-test car&cdr-tr 'a '(b c d () e (a)))
(eval-test car&cdr-tr 'a '(() () a))

; (time (car&cdr 'a '(b c d e f g (h i j k l (m n o p q r s (t u (v w) (x y))) (z a))) 'fail))
; (time (car&cdr-try1 'a '(b c d e f g (h i j k l (m n o p q r s (t u (v w) (x y))) (z a))) 'fail))
; (time (car&cdr-tr 'a '(b c d e f g (h i j k l (m n o p q r s (t u (v w) (x y))) (z a))) 'fail))

(define (build-composition p)
  (if (null? (cdr p)) (car p)
  (list 'compose (car p) (build-composition (cdr p)))))

(define (car&cdr2 s slst errvalue)
  (if (equal? (visit-pattern s slst errvalue) errvalue) errvalue
  (build-composition (reverse (visit-pattern s slst errvalue)))))

(equal? (car&cdr2 'a '(a b c) 'fail) 'car)
(equal? (car&cdr2 'c '(a b c) 'fail) '(compose car (compose cdr cdr)))
(equal? (car&cdr2 'dog '(cat lion (fish dog)) 'fail) '(compose car (compose cdr (compose car (compose cdr cdr)))))
(equal? (car&cdr2 'a '(b c) 'fail) 'fail)
(equal? (car&cdr2 'a '((a)) 'fail) '(compose car car))
(equal? (car&cdr2 'a '(() () () a) 'fail) '(compose car (compose cdr (compose cdr cdr))))

(define (build-application-list lst)
  (if (null? lst) (lambda (x) x)
      (lambda (val) ((car lst) ((build-application-list (cdr lst)) val)))))

(define compose (lambda x (build-application-list x)))


(define (makezero x) 0)
(define (add1 x) (+ 1 x))
(define (double x) (* 2 x))

(equal? ((compose) '(a b c d)) '(a b c d))
(equal? ((compose car) '(a b c d)) 'a)
(equal? ((compose car cdr cdr) '(a b c d)) 'c)
(equal? ((compose double add1 makezero) 1) 2)

(define (lt a b) (< a b))

(define (insert pred el lst)
  (if (null? lst) (list el)
      (if (pred el (car lst))
          (cons el (cons (car lst) (cdr lst)))
          (cons (car lst) (insert pred el (cdr lst))))))

(define (sort lon)
  (if (null? lon) '()
     (insert lt (car lon) (sort (rest lon)))))

(equal? (sort '()) '())
(equal? (sort '(1)) '(1))
(equal? (sort '(1 2)) '(1 2))
(equal? (sort '(2 1)) '(1 2))
(equal? (sort '(1 3 2)) '(1 2 3))
(equal? (sort '(2 1 3)) '(1 2 3))
(equal? (sort '(3 1 2)) '(1 2 3))
(equal? (sort '(3 2 1)) '(1 2 3))
(equal? (sort '(8 2 5 2 3)) '(2 2 3 5 8))

(define (sort-predicate pred lon)
  (if (null? lon) '()
      (insert pred (car lon) (sort-predicate pred (rest lon)))))

(equal? (sort-predicate < '(8 2 5 2 3)) '(2 2 3 5 8))
(equal? (sort-predicate > '(8 2 5 2 3)) '(8 5 3 2 2))
(equal? (sort-predicate > '()) '())
(equal? (sort-predicate > '(3 2 1)) '(3 2 1))
(equal? (sort-predicate < '(1 1)) '(1 1))

;; Figured out yet another idea in which you can return a function which will be applied to yield
;; reverse nesting.

#|
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
      (let ((result (access-pattern s sexp errvalue)))
      (if (equal? result errvalue) errvalue result))))

(define (access-pattern s slst errvalue)
  (if (null? slst) errvalue
          (join-scans (scan-sexp s (car slst) errvalue)
                      (scan-sexp s (cdr slst) errvalue) errvalue)))

(define (car&cdr-fn-helper s slst errvalue init)
  (let ((result (access-pattern s slst errvalue)))
  (if (equal? result errvalue) errvalue
      (list 'lambda '(lst) (result init)))))

(define (car&cdr-fn s slst errvalue)
  ((car&cdr-fn-helper s slst errvalue) '()))

(equal? (car&cdr-fn 'a '() 'fail) 'fail)
(eval-test car&cdr-fn 'a '(() ((a))))
(eval-test car&cdr-fn 'a '(() (a)))
(eval-test car&cdr-fn 'a '(() (b) a))
(eval-test car&cdr-fn 'a '(b c d () e (a)))
(eval-test car&cdr-fn 'a '(() () a)))
|#