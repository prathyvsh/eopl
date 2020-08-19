#lang scheme

(define (remove-first s los)
  (if (null? los) '()
      (if (eq? (car los) s)
          (cdr los)
          (cons (car los) (remove-first s (cdr los))))))

(equal? (remove-first 'apple '(apple orange lemon)) '(orange lemon))

;; If the first occurrence of cons is replaced with remove-first the
;; procedure would compute the segment of the list after the first occurrence of s.


(define (remove-first-new s los)
  (if (null? los) '()
      (if (eq? (car los) s)
          (cdr los)
          (remove-first-new s (cdr los)))))

(equal? (remove-first-new 'apple '(apple orange lemon)) '(orange lemon))
(equal? (remove-first-new 'apple '(lemon orange 'apple)) '())
(equal? (remove-first-new 'apple '(kiwi orange apple lemon)) '(lemon))