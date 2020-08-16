#lang scheme

(define curry2
  (lambda (proc)
  (lambda (arg1)
    (lambda (arg2)
      (proc arg1 arg2)))))

(equal? (((curry2 +) 1) 2) 3)

(define consa ((curry2 cons) 'a))
(equal? (consa '(b)) '(a b))