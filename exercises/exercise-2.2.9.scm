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

(define (build-selector s slst errvalue)
    (if (null? slst) errvalue
          (if (equal? s (car slst)) '(lst)
                 (if (equal? (build-selector s (cdr slst) errvalue) errvalue)
                     errvalue
                     (cons 'cdr (build-selector s (cdr slst) errvalue))))))

(define (process-exp s exp errvalue)
  (if (symbol? exp)
      (if (equal? s exp) 'lst errvalue)
       (if (null? exp) errvalue
           (if (equal? (process-exp s (car exp) errvalue) errvalue)
               (if (equal? (car&cdr-helper s (cdr exp) errvalue) errvalue)
                   errvalue
                   (list 'cdr (car&cdr-helper s (cdr exp) errvalue)))
               (list 'car (process-exp s (car exp) errvalue))))))

;; s-exp: symbol | s-list
;; s-list: ({<s-exp>}*)
(define (car&cdr-helper s slst errvalue)
  (if (null? slst) errvalue
  (if (equal? (process-exp s (car slst) errvalue) errvalue)
      (if (equal? (car&cdr-helper s (cdr slst) errvalue) errvalue)
          errvalue
          (list 'cdr (car&cdr-helper s (cdr slst) errvalue)))
      (process-exp s (car slst) errvalue))))

(define (car&cdr s slst errvalue)
  (if (equal? (car&cdr-helper s slst errvalue) errvalue)
      errvalue
      (list 'lambda '(lst) (list 'car (car&cdr-helper s slst errvalue)))))
      

(equal? (car&cdr 'a '(a b c) 'fail) '(lambda (lst) (car lst)))
(equal? (car&cdr 'a '() 'fail) 'fail)
(equal? (car&cdr 'a '(() () () a) 'fail) '(lambda (lst) (car (cdr (cdr (cdr lst))))))
(equal? (car&cdr 'c '(a b c) 'fail) '(lambda (lst) (car (cdr (cdr lst)))))
(equal? (car&cdr 'apple '((apple)) 'fail) '(lambda (lst) (car (car lst))))
(equal? (car&cdr 'dog '(cat lion (fish dog) pig) 'fail) '(lambda (lst)
                                                           (car (cdr (car (cdr (cdr lst)))))))
(equal? (car&cdr 'a '(b c) 'fail) 'fail)