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