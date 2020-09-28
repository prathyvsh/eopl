#lang scheme

(define x '(a b ((3) c) d))

(eq? (car (cdr x)) 'b)

(equal? (caddr x) '((3) c))

(equal? (cdaddr x) '(c))

(eq? (char? (car '(#\a #\b))) #t)

(equal? (cons 'x x) '(x a b ((3) c) d))

(equal? (cons (list 1 2) (cons 3 '(4))) '((1 2) 3 4))

(equal? (cons (list) (list 1 (cons 2 '()))) '(() 1 (2)))