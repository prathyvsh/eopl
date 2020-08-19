#lang scheme

(define (remove s los)
  (if (null? los) '()
      (if (eq? s (car los)) (remove s (cdr los))
          (cons (car los) (remove s (cdr los))))))

(equal? (remove 'apple '(apple apple orange orange lemon lemon)) '(orange orange lemon lemon))
(equal? (remove 'apple '(orange apple lemon apple)) '(orange lemon))

(define (remove-new s los)
  (if (null? los) '()
      (if (eq? s (car los)) (remove-new s (cdr los))
          (remove-new s (cdr los)))))

;; This would result in a function that always return null.
(equal? (remove-new 'apple '(apple orange lemon)) '())
(equal? (remove-new 'apple '(orange lemon)) '())