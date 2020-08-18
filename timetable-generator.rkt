#lang racket

(require racket/date)
(require racket/format)

(define (stamp hour min day month year)
  (date 00 min hour day month year 0 0 0 19800))

(define (in-minutes seconds)
  (/ seconds 60))

(define (span start end)
  (in-minutes (- (date->seconds end) (date->seconds start))))

(define (build-row entry)
  (let ((start (second entry))
        (end (third entry)))
  (~a (first entry) " " (date->string start) " - " (date->string end) " " (span start end) " minutes" "\n")))

(define (build-section section)
  (~a (first section) "\n"
  (map build-row (second section))))

(define (build-table entries)
  (map build-section entries))

(define dates
  (list
   (list "Overview"
  (list
   (list "Overview" (stamp 00 44 6 8 2020) (stamp 01 14 6 8 2020))
   (list "Overview" (stamp 3 55 6 8 2020) (stamp 4 25 6 8 2020))))
   (list "Section 1"
         (list
          (list "Section 1.1" (stamp 21 30 30 7 2020) (stamp 22 00 30 7 2020))
          (list "Exercise 1.1.1" (stamp 23 20 15 8 2020) (stamp 22 20 15 8 2020))
          (list "Section 1.1" (stamp 23 00 15 8 2020) (stamp 22 30 15 8 2020) 8)
          (list "Section 1.2 - 15" (stamp 1 00 16 8 2020) (stamp 1 30 16 8 2020) 15)
          (list "Exercise 1.2.1" (stamp 3 01 16 8 2020) (stamp 3 16 16 8 2020))
          (list "Section 1.2" (stamp 3 16 16 8 2020) (stamp 3 31 16 8 2020) 18)
          (list "Exercise 1.2.2" (stamp 4 40 16 8 2020) (stamp 4 54 16 8 2020))
          (list "Section 1.2" (stamp 4 54 16 8 2020) (stamp 5 10 16 8 2020) 21)
          (list "Exercise 1.2.3" (stamp 15 30 16 8 2020) (stamp 15 35 16 8 2020))
          (list "Section 1.2" (stamp 15 35 16 8 2020) (stamp ) 25)
          (list "Exercise 1.3.1" (stamp) (stamp))
          (list "Exercise 1.3.2 - 1.3.3" (stamp) (stamp))
          (list "Section 1.3" (stamp) (stamp) 28)
          (list "Exercise 1.3.4 - 1.3.6" (stamp) (stamp))
          (list "Section 1.3" (stamp) (stamp) 29)
          (list "Execise 1.3.7" (stamp) (stamp))))
                
          
      
   ))

(display (~a (build-table dates)))



