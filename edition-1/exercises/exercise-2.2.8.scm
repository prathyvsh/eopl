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
(equal? (up '((1) (2) (apple) (3) ((4 5))))  '(1 2 apple 3 (4 5)))

(define (count-occurrences s slst)
  (if (null? slst) 0
       (+ (if (list? (car slst)) (count-occurrences s (car slst))
           (if (equal? s (car slst)) 1
               0))
          (count-occurrences s (cdr slst)))))

(equal? (count-occurrences 'apple '()) 0)
(equal? (count-occurrences 'orange '(orange)) 1)
(equal? (count-occurrences 'x '((f x) y (((x z) x)))) 3)
(equal? (count-occurrences 'w '((f x) y (((x z) x)))) 0)
(equal? (count-occurrences 'orange '((((((orange))))))) 1)

(define (flatten slst)
  (if (null? slst) '()
      (if (list? (car slst)) (append (flatten (car slst)) (flatten (cdr slst)))
      (cons (car slst) (flatten (cdr slst))))))

(equal? (flatten '(a b c)) '(a b c))
(equal? (flatten '((a b) c (((d)) e))) '(a b c d e))
(equal? (flatten '(a b (() (c)))) '(a b c))

(define (merge lon1 lon2)
  (if (null? lon1) lon2
      (if (null? lon2) lon1
          (if (< (car lon1) (car lon2))
              (cons (car lon1) (merge (cdr lon1) lon2))
              (cons (car lon2) (merge lon1 (cdr lon2)))))))

(equal? (merge '(1 4) '(1 2 8)) '(1 1 2 4 8))
(equal? (merge '() '(1 2 3)) '(1 2 3))
(equal? (merge '(1 2 3) '()) '(1 2 3))
(equal? (merge '(10) '(20)) '(10 20))
(equal? (merge '(35 62 81 90 91) '(3 83 85 90)) '(3 35 62 81 83 85 90 90 91))