#lang scheme

(define (subst-with-map new old s-list)
  (map (lambda (s)
         (if (symbol? s)
         (if (equal? s old) new s)
         (subst-with-map new old s)))
         s-list))

(equal? (subst-with-map 'a 'b '((b c) (b d))) '((a c) (a d)))
(equal? (subst-with-map 'a 'b '((b c) b b (b d))) '((a c) a a (a d)))