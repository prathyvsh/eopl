#lang scheme

(define (subst-symbol-expression new old sexp)
  (if (symbol? sexp)
      (if (eq? sexp old) new sexp)
      (subst new old sexp)))

(define (subst new old s-list)
  (if (null? s-list) '()
      (cons (subst-symbol-expression new old (car s-list))
            (subst new old (cdr s-list)))))

(equal? (subst 'a 'b '((b c) (b d))) '((a c) (a d)))

;; Here the recursion is guaranteed to halt even when using
;; sexp in the last expression of subst-symbol-expression
;; because it calls subst on it which is guaranteed to
;; make the structure being passed to it smaller or return empty
;; list on each call.