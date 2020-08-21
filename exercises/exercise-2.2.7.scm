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
      
(define (filter-in p lst)
  (if (null? lst) '()
      (if (p (car lst)) (cons (car lst)
                        (filter-in p (cdr lst)))
          (filter-in p (cdr lst)))))

(equal? (filter-in number? '(a 2 (1 3) b 7))'(2 7))
(equal? (filter-in symbol? '(a (b c) 17 foo)) '(a foo))
(equal? (filter-in (lambda (x) (and (number? x)
                                (equal? (remainder x 2)
                                        0)))
                '(1 2 3 4 (4 5) 7 9 8 1 2))
        '(2 4 8 2))

(define (all-combos sym los)
  (if (null? los)
      '()
      (cons (list sym (car los))
            (all-combos sym (cdr los)))))

(define (product los1 los2)
  (if (null? los1) '()
      (append (all-combos (car los1) los2)
      (product (cdr los1) los2))))

(equal? (product '() '()) '())
(equal? (product '(a b) '()) '())
(equal? (product '(a) '(b)) '((a b)))
(equal? (product '(x) '(a b)) '((x a) (x b)))
(equal? (product '(a b) '(x)) '((a x) (b x)))
(equal? (product '(a b c) '(x y)) '((a x) (a y) (b x) (b y) (c x) (c y)))

(define (swap s1 s2 sym-or-list)
  (if (symbol? sym-or-list)
      (if (equal? sym-or-list s1) s2
      (if (equal? sym-or-list s2) s1
      sym-or-list))
      (swapper s1 s2 sym-or-list)))

(define (swapper s1 s2 slst)
  (if (null? slst) '()
      (cons (swap s1 s2 (car slst)) (swapper s1 s2 (cdr slst)))))

(equal? (swapper 'a 'd '(a b c d)) '(d b c a))
(equal? (swapper 'x 'y '((x) y (z (x))))
        '((y) x (z (y))))
(equal? (swapper 'a 'b '()) '())
(equal? (swapper 'a 'b '(b a)) '(a b))

(define (rotate los)
  (if (null? los) '()
      (append (list (last-item los))
              (but-last los))))

(define (last-item los)
  (if (null? los) '()
      (if (null? (cdr los)) (car los)
      (last-item (cdr los)))))

(define (but-last los)
  (if (null? los) '()
      (if (null? (cdr los)) '()
      (cons (car los) (but-last (cdr los))))))

(equal? (rotate '(a b c d)) '(d a b c))
(equal? (rotate '(notmuch)) '(notmuch))
(equal? (rotate '()) '())
(equal? (rotate '(a b)) '(b a))
(equal? (rotate (rotate '(a b))) '(a b))