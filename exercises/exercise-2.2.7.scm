#lang scheme

;; s: symbol
;; n: non-negative integer
;; lst: list
;; v: vector
;; los: list of symbols
;; vos: vector of symbols
;; slst: s-list
;; x: object
;; s1: symbol
;; los: list of symbols
;; x1: object

(define (duple n x)
  (if (= n 0) '()
      (cons x (duple (- n 1) x))))

(equal? (duple 2 3) '(3 3))
(equal? (duple 4 '(ho ho)) '((ho ho) (ho ho) (ho ho) (ho ho)))
(equal? (duple 0 '(blah)) '())
(equal? (duple 1 'apple) '(apple))
(equal? (duple 1 '(blah)) '((blah)))

(define (invert lst)
  (if (null? lst) '()
     (cons (cons (cadar lst) (list (caar lst)))
     (invert (cdr lst)))))

(equal? (invert '((x y))) '((y x)))
(equal? (invert '((a 1) (a 2) (b 1) (b 2)))
        '((1 a) (2 a) (1 b) (2 b)))
  
(define (list-index s los)
  (find-index s los 0))

(define (find-index s los counter)
  (if (null? los) -1
      (if (equal? s (car los)) counter
          (find-index s (cdr los) (+ 1 counter)))))
          

(equal? (list-index 'c '(a b c d)) 2)
(equal? (list-ref '(a b c) (list-index 'b '(a b c))) 'b)
(equal? (list-index 'avocado '(beans peas)) -1)

(define (vector-index s vos)
      (find-vec-index s vos 0))

(define (find-vec-index s vos counter)
  (if (>= counter (vector-length vos)) -1
      (if (equal? s (vector-ref vos counter))
          counter
          (find-vec-index s vos (+ 1 counter)))))

(equal? (vector-index 'c '#(a b c d)) 2)
(equal? (vector-ref '#(a b c) (vector-index 'b '#(a b c))) 'b)
(equal? (vector-index 'apple '#(a p p l e)) -1)

(define (ribassoc s los v fail-value)
  (if (equal? (list-index s los) -1)
      fail-value
      (vector-ref v (list-index s los))
  ))

(equal? (ribassoc 'b '(a b c) '#(1 2 3) 'fail) 2)
(equal? (ribassoc 'c '(a b foo) '#(3 squiggle bar) 'fail) 'fail)
(equal? (ribassoc 'i '(a i o i) '#(fx (fx) () (fn fe)) 'fail) '(fx))

(define (filter-item p datum-or-lst)
  (if (list? datum-or-list)
      (filter-in p datum-or-list)
      (if (equal? p datum-or-list) p '())))
      
(define (filter-in p lst)
  (if (null? lst) '()
      (cons (filter-item p (car lst))
            (filter-in p (cdr lst)))))