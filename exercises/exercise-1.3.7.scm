#lang scheme

(define compose (lambda x
                  (if (null? (cddr x))
                      (lambda (val) ((cadr x) ((car x) val)))
                      (if (null? (cdddr x))
                          (lambda (val) ((caddr x) ((cadr x) ((car x) val))))
                          (error "Please provide 2 or 3 arguments for composition")))))

(define (double x) (* 2 x))
(define (triple x) (* 3 x))
(define (quadruple x) (* 4 x))

((compose double triple) 1)
((compose triple quadruple) 1)
((compose double triple quadruple) 2)
((compose double triple quadruple double) 0)

;; A better approach is to use a map or reduce so that generality is acquired.
;; But since this is an exercise under the section of variable arity, that
;; idea is demonstrated