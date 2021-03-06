#lang racket

(require racket/date)

(define (stamp hour min day month year)
  (date* 00 min hour day month year 0 0 0 19800 0 "IST"))

(define dates
  (list
   (list "Overview"
  (list
   (list "Introduction" (stamp 00 29 29 9 2020) (stamp 00 39 29 9 2020))))))