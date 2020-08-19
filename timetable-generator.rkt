#lang racket

(require racket/date)
(require racket/format)

(define (stamp hour min day month year)
  (date 00 min hour day month year 0 0 0 19800))

(define (in-minutes seconds)
  (/ seconds 60))

(define (in-hours-and-minutes minutes)
  (let ((h (quotient minutes 60))
        (m (remainder minutes 60)))
    (~a h (if (eq? h 1) " hour" " hours")
        (if (eq? m 0) "" (~a " " m (if (eq? m 1) " minute" " minutes"))))))

(define (span start end)
  (in-minutes (- (date->seconds end) (date->seconds start))))

(define (build-row entry)
  (let* ((start (second entry))
        (end (third entry))
        (timespan (span start end)))
  (list
   (~a (~a "| " (first entry) #:min-width 24) " | " (~a timespan " minutes" #:min-width 20) "|")
   timespan)))

(define (build-section section)
  (let* ((rows (map build-row (second section)))
         (row-entry (map first rows))
         (time (map second rows))
         (total-time (foldl + 0 time)))
    (list (~a "| " (~a "*" (first section) "*" #:min-width 22) " | *" (in-hours-and-minutes total-time) "* |\n"
  (string-join row-entry "\n")) total-time)))

(define (build-table entries)
  (let* ((result (map build-section entries))
         (entries (map first result))
         (time (map second result)))
    (~a "* Essentials of Programming Languages\n"
        "** Total Time: " (in-hours-and-minutes (foldl + 0 time)) "\n"
        (string-join entries "\n"))))

(define dates
  (list
   (list "Overview"
  (list
   (list "Overview" (stamp 00 44 6 8 2020) (stamp 01 14 6 8 2020))
   (list "Overview" (stamp 3 55 6 8 2020) (stamp 4 25 6 8 2020))))
   (list "Section 1"
         (list
          (list "Section 1.1" (stamp 21 30 30 7 2020) (stamp 22 00 30 7 2020))
          (list "Exercise 1.1.1" (stamp 22 20 15 8 2020) (stamp 22 20 15 8 2020))
          (list "Section 1.1" (stamp 23 00 15 8 2020) (stamp 23 30 15 8 2020) 8)
          (list "Section 1.2 - 15" (stamp 1 00 16 8 2020) (stamp 1 30 16 8 2020) 15)
          (list "Exercise 1.2.1" (stamp 3 01 16 8 2020) (stamp 3 16 16 8 2020))
          (list "Section 1.2" (stamp 3 16 16 8 2020) (stamp 3 31 16 8 2020) 18)
          (list "Exercise 1.2.2" (stamp 4 40 16 8 2020) (stamp 4 54 16 8 2020))
          (list "Section 1.2" (stamp 4 54 16 8 2020) (stamp 5 10 16 8 2020) 21)
          (list "Exercise 1.2.3" (stamp 15 30 16 8 2020) (stamp 15 35 16 8 2020))
          (list "Section 1.2" (stamp 15 35 16 8 2020) (stamp 16 00 16 8 2020) 25)
          (list "Exercise 1.3.1" (stamp 16 00 16 8 2020) (stamp 16 30 16 8 2020))
          (list "Exercise 1.3.2 - 1.3.3" (stamp 17 16 16 8 2020) (stamp 17 21 16 8 2020))
          (list "Section 1.3" (stamp 17 21 16 8 2020) (stamp 17 46 16 8 2020) 28)
          (list "Exercise 1.3.4 - 1.3.6" (stamp 17 48 16 8 2020) (stamp 17 55 16 8 2020))
          (list "Section 1.3" (stamp 17 55 16 8 2020) (stamp 18 18 16 8 2020) 29)
          (list "Exercise 1.3.7" (stamp 16 21 17 8 2020) (stamp 16 32 17 8 2020))))
   (list "Section 2"
         (list
          (list "Section 2.1" (stamp 16 32 17 8 2020) (stamp 16 51 17 8 2020) 34)
          (list "Section 2.1" (stamp 17 07 17 8 2020) (stamp 17 12 17 8 2020) 35)
          (list "Exercise 2.1.1" (stamp 17 12 17 8 2020) (stamp 17 13 17 8 2020))
          (list "Section 2.1" (stamp 17 13 17 8 2020) (stamp 17 14 17 8 2020) 36)
          (list "Exercise 2.1.2" (stamp 17 14 17 8 2020) (stamp 17 37 17 8 2020))
          (list "Exercise 2.1.2" (stamp 18 00 17 8 2020) (stamp 18 30 17 8 2020))
          (list "Exercise 2.1.3" (stamp 1 50 19 8 2020) (stamp 2 20 19 8 2020))
          (list "Section 2.1.4" (stamp 22 39 19 8 2020) (stamp 23 09 19 8 2020) 38)
          (list "Section 2.2.1" (stamp 00 41 20 8 2020) (stamp 1 11 20 8 2020) 44)
          (list "Exercise 2.2.1" (stamp 2 05 20 8 2020) (stamp 2 35 20 8 2020))
)
                                                                                
          )
   (list "Extras"
         (list (list "Reading List" (stamp 21 12 18 8 2020) (stamp 21 42 18 8 2020))
         (list "Reading List" (stamp 23 21 18 8 2020) (stamp 23 51 18 8 2020))
         (list "Building Timetable Generator" (stamp 2 20 19 8 2020) (stamp 2 50 19 8 2020))
         (list "Building Timetable Generator" (stamp 2 50 19 8 2020) (stamp 3 20 19 8 2020))
         (list "Building Timetable Generator" (stamp 3 20 19 8 2020) (stamp 3 50 19 8 2020))
         (list "Building Timetable Generator" (stamp 3 50 19 8 2020) (stamp 4 20 19 8 2020))
   ))))

;; TODO:
;; Breakdown of reading speed by every 100 pages:
;; 100/463 pages read in n pomodoros
;; 200/463 pages read in m pomodoros
;; Reading speed
;; Breakdown of time spent for exercises (by chapter and by 50 pomodoros?)

(with-output-to-file "./timetable.org" (lambda () (display (build-table dates))) #:exists 'replace)


