#lang scheme

(define (down l)
  (if (null? l) '()
      (cons (list (car l))
                  (down (cdr l)))))

(equal? (down '(1 2 3)) '((1) (2) (3)))
(equal? (down '(a (more (complicated)) object))
        '((a) ((more (complicated))) (object)))
(equal? (down '()) '())

(define (up lst)
  (if (null? lst) '()
      (if (list? (car lst))
          (append
           (car lst)
           (up (cdr lst)))
          (cons (car lst) (up (cdr lst))))))

(equal? (up '((1 2) (3 4))) '(1 2 3 4))
(equal? (up '((x (y)) z)) '(x (y) z))