#lang scheme

((lambda (x)
   (list x (list (quote quote) x)))
 (quote (lambda (x)
          (list x (list (quote quote) x)))))

(equal? ((lambda (x)
   (list x (list (quote quote) x)))
 (quote (lambda (x)
          (list x (list (quote quote) x))))) '((lambda (x)
   (list x (list (quote quote) x)))
 (quote (lambda (x)
          (list x (list (quote quote) x))))))

;; This definition on executing generates a quoted literal version of itself
;; which makes it a Quine.

;; Similar behaviour can be achieved without list procedure using cons
;; to recreate similar structure in its place.

;; Though without using cons it seems impossible as using an application to produce
;; a Quine which can be reapplied, the first term used for application would have to be
;; reconstructed in the result which necessitates the use of list for replicating the
;; application structure as a literal.

;; That is, even a simple application such as (lambda (x) x) is applied to
;; itself, reconstructing it as a literal would require the list procedure
;; to contain it.