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
          (if (equal? s (car slst))'(car lst)
                 (if (equal? (build-selector s (cdr slst) errvalue) errvalue)
                     errvalue
                     (list 'cdr (build-selector s (cdr slst) errvalue))))))

(define (car&cdr s slst errvalue)
  (if (equal? (build-selector s slst errvalue) errvalue) errvalue
      (list 'lambda `(lst) (build-selector s slst errvalue))))

(equal? (car&cdr 'a '(a b c) 'fail) '(lambda (lst) (car lst)))
(equal? (car&cdr 'c '(a b c) 'fail) '(lambda (lst) (car (cdr (cdr lst)))))
(equal? (car&cdr 'dog '(cat lion (fish dog) pig) 'fail) '(lambda (lst)
                                                           (car (cdr (car (cdr (cdr lst)))))))
(equal? (car&cdr 'a '(b c) 'fail) 'fail)