#lang scheme

(lambda (x) (lambda (y) ((lambda (x) (x y)) x)))
;; Let the whole equation be called p
;; and the inner application be called (q r)
;; Here the binding x in p is associated with x in r.
;; The binding y in p is associated with y in q.
;; And the binding x in q is associated with the x occuring inside it.

(lambda (x) ((lambda (a b c) (a (lambda (a) (+ a c)) b))
  (lambda (f x) (f (z x)))))

;; Let this expression be denoted as:
;; (λ (x) ((λ (a b c) (a p b))
;;        (λ (f x) m)))

;; Now the first binding of x isn't associated with any binding
;; The a b and c are respectively associated with its inner a, inner b
;; and c in p, but not with the a in p. The a in p is the a bound in p.
;; The f and x binding is associated with f and x in m.