#lang scheme

;; s-exp: symbol | s-list
;; s-list: ({<s-exp>}*)
;; There is a reversal of operations happening when seeking
;; for the match
(define (visit-pattern s slst errvalue)
  (if (null? slst) errvalue
      (if (symbol? slst)
          (if (eq? s slst) '() errvalue)
          (if (eq? (visit-pattern s (car slst) errvalue) errvalue)
              (if (eq? (visit-pattern s (cdr slst) errvalue) errvalue)
                  errvalue
                  (cons 'cdr (visit-pattern s (cdr slst) errvalue)))
                  (cons 'car (visit-pattern s (car slst) errvalue))))))

(define (reverse l)
  (if (null? l) '()
      (append (reverse (cdr l)) (list (car l)))))

(define (nest l)
  (if (null? l) '()
      (if (null? (cdr l)) (car l)
      (list (car l) (nest (cdr l))))))

(define (build-composition p)
  (if (null? (cdr p)) (car p)
  (list 'compose (car p) (build-composition (cdr p)))))

(define (car&cdr2 s slst errvalue)
  (if (equal? (visit-pattern s slst errvalue) errvalue) errvalue
  (build-composition (reverse (visit-pattern s slst errvalue)))))

(equal? (car&cdr2 'a '(a b c) 'fail) 'car)
(equal? (car&cdr2 'c '(a b c) 'fail) '(compose car (compose cdr cdr)))
(equal? (car&cdr2 'dog '(cat lion (fish dog)) 'fail) '(compose car (compose cdr (compose car (compose cdr cdr)))))
(equal? (car&cdr2 'a '(b c) 'fail) 'fail)
(equal? (car&cdr2 'a '((a)) 'fail) '(compose car car))
(equal? (car&cdr2 'a '(() () () a) 'fail) '(compose car (compose cdr (compose cdr cdr))))