(define partial-vector-sum
  (lambda (von n)
    (if (zero? n) 0
        (+ (vector-ref von (- n 1))
           (partial-vector-sum von (- n 1))))))


;; IH(0) = (partial-vector-sum von 0)
;; Whenever n is zero, we have the answer as 0
;; Now let IH(k) denote (partial-vector-sum von k) to terminate and
;; and give the correct answer.
;; Now for a value 0 < k + 1 < length(von)
;; We have IH(k + 1) is given by (+ (vector ref k) (partial-vector-sum von k))
;; We have from IH(k) that IH(k) terminates and gives the right answer.
;; So, IH(k + 1) needs (vector ref k) to be give the correct answer which would
;; always hold for 0 < k + 1 < length(von). Thus with both terms providing the
;; correct answer IH(k + 1) is proven to both terminate and give the correct answer.