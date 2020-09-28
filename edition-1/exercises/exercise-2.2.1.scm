#lang scheme

(define (list-of-numbers? l)
  (or (null? l)
      (and (pair? l)
            (number? (car l)) (list-of-numbers? (cdr l)))))


(eq? (list-of-numbers? 3) #f)
(eq? (list-of-numbers? '()) #t)
(eq? (list-of-numbers? '(1 2 3)) #t)
(eq? (list-of-numbers? '(a b c)) #f)
(eq? (list-of-numbers? '(1 a 2)) #f)

;; Here the list-of-numbers? positively checks for all the cases in which a list
;; can occur, in this case, even if a symbol is passed, since it will fail both null? and pair?
;; tests the list-of-numbers will correctly return false.

(eq? (list-of-numbers? 'apple) #f)

(define (nth-elt l idx)
  (if (null? l) (error "List too short to locate the index")
  (if (eq? idx 0)
      (car l)
      (nth-elt (cdr l) (- idx 1)))))

(eq? (nth-elt '(1 2 3) 1) 2)
(eq? (nth-elt '(2 4 5) 2) 5)
(eq? (nth-elt '(1) 0) 1)

;; In the case of nth-elt, it is important that a positive check for l is performed.
;; This can be done with the help of a guard for list? in the beginning of the procedure as:

(define (guarded-nth-elt l idx)
  (if (list? l)
      (nth-elt l idx)
      (error "Please provide a list")))

;; list-ref
(define (guarded-list-ref l n) (if (list? l) (list-ref l n) (error "Please provide a list")))
;; length
(define (length l) (if (list? l) (length l) (error "Please provied a list")))

;; Q. Can a sensible value be returned sometimes?
;; A. Yes, a sensible value can be returned in the case of certain operations.
;; A behaviour like this can be found in Javascript, when you try to find the
;; index of a non-existent character is provided, a -1 value is returned.
;; Similarly, a boolean false can be returned in the case of certain operations
;; when a search or operation fails.

;; When is it worth the effort to check that arguments are of the right type? Why?
;; In a dynamically typed language, it is worth checking the type in cases where a
;; function can be expected to be given an elaborate data structure, the parts of
;; which can sometimes violate the requisite type. Also, when there’s a communication
;; gap between between the producer of the library and the consumer, the library author
;; by understanding the use case of the consumer could also provide dynamic type checking
;; at critical junctures of the codebase.