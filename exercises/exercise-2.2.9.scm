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

(time (car&cdr-try1 'a '(b c d e f g (h i j k l (m n o p q r s (t u (v w) (x y))) (z a))) 'fail))

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

(define (join-scans car-scan cdr-scan errvalue)
  (if (and (equal? car-scan errvalue) (equal? cdr-scan errvalue))
      errvalue
      (if (equal? car-scan errvalue)
          (append cdr-scan '(cdr))
          (append car-scan '(car)))))

(define (scan-sexp s sexp errvalue)
  (if (symbol? sexp)
      (if (equal? s sexp)'(lst) errvalue)
      (if (equal? (access-pattern s sexp errvalue) errvalue)
          errvalue
          (access-pattern s sexp errvalue))))

(define (access-pattern s slst errvalue)
  (if (null? slst) errvalue
          (join-scans (scan-sexp s (car slst) errvalue)
                      (scan-sexp s (cdr slst) errvalue) errvalue)))

(define (car&cdr s slst errvalue)
  (if (equal? (access-pattern s slst errvalue) errvalue) errvalue
      (list 'lambda '(lst) (access-pattern s slst errvalue))))

(car&cdr 'a '() 'fail)
(car&cdr 'a '(() ((a))) 'fail)
(car&cdr 'a '(() (a)) 'fail)
(car&cdr 'a '(() (b) a) 'fail)
(car&cdr 'a '(b c d () e (a)) 'fail)
(car&cdr 'a '(() () a) 'fail)