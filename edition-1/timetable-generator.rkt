#lang racket

(require racket/date)
(require racket/format)
(require racket/dict)

(define (stamp hour min day month year)
  (date* 00 min hour day month year 0 0 0 19800 0 "IST"))

(define (in-minutes seconds)
  (/ seconds 60))

(define (format-time h m)
  (if (and (eq? h 0) (eq? m 0)) "0 minutes"
      (let ((hours (~a h " " (if (eq? h 1) "hour" "hours")))
            (minutes (~a m " " (if (eq? m 1) "minute" "minutes"))))
        (if (eq? h 0) minutes
            (if (eq? m 0) hours
                (string-join (list hours minutes) " "))))))

(define (in-hours-and-minutes minutes)
  (let ((h (quotient minutes 60))
        (m (remainder minutes 60)))     
          (format-time h m)))

(define (span start end)
  (let ((timespan (- (date->seconds end) (date->seconds start))))
    (if (< timespan 0) (error "Please provide a proper start and end date")
        timespan)))

(define (parse-row entry)
  (let* ((start (second entry))
        (end (third entry))
        (timespan (in-minutes (span start end))))
    (list (first entry) timespan)))

(define (build-row entry)
  (let* ((row (parse-row entry))
         (title (first row))
         (timespan (second row)))
  (list
   (~a (~a "| " title #:min-width 24) " | " (~a timespan " minutes" #:min-width 20) "|")
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
 (~a (~a "** Total Time: " (in-hours-and-minutes (foldl + 0 time)) "\n")
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
          (list "Section 1.2" (stamp 1 00 16 8 2020) (stamp 1 30 16 8 2020) 15)
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
          (list "Section 2.2.2" (stamp 3 38 20 8 2020) (stamp 3 42 20 8 2020) 46)
          (list "Exercise 2.2.2" (stamp 3 42 20 8 2020) (stamp 3 53 20 8 2020))
          (list "Section 2.2.2" (stamp 3 53 20 8 2020) (stamp 4 08 20 8 2020) 47)
          (list "Section 2.2.2" (stamp 00 56 21 8 2020) (stamp 1 06 21 8 2020) 49)
          (list "Exercise 2.2.3 - Exercise 2.2.4" (stamp 01 06 21 8 2020) (stamp 01 16 21 8 2020))
          (list "Section 2.2.2" (stamp 01 16 21 8 2020) (stamp 01 26 21 8 2020) 50)
          (list "Exercise 2.2.6" (stamp 21 17 21 8 2020) (stamp 21 47 21 8 2020))
          (list "Exercise 2.2.7" (stamp 22 39 21 8 2020) (stamp 23 09 21 8 2020))
          (list "Exercise 2.2.7" (stamp 23 30 21 8 2020) (stamp 00 00 22 8 2020))
          (list "Exercise 2.2.7" (stamp 00 50 22 8 2020) (stamp 01 10 22 8 2020))
          (list "Exercise 2.2.8" (stamp 01 10 22 8 2020) (stamp 01 20 22 8 2020))
          (list "Exercise 2.2.8" (stamp 21 06 23 8 2020) (stamp 21 27 23 8 2020))
          (list "Exercise 2.2.9 - 1" (stamp 21 27 23 8 2020) (stamp 21 36 23 8 2020))
          (list "Exercise 2.2.9 - 1" (stamp 21 46 23 8 2020) (stamp 22 16 23 8 2020))
          (list "Exercise 2.2.9 - 2" (stamp 23 10 23 8 2020) (stamp 23 40 23 8 2020))
          (list "Exercise 2.2.9 - 2" (stamp 23 46 23 8 2020) (stamp 00 16 24 8 2020))
          (list "Exercise 2.2.9 - 2" (stamp 02 27 24 8 2020) (stamp 02 57 24 8 2020))
          (list "Exercise 2.2.9 - 2" (stamp 02 57 24 8 2020) (stamp 03 27 24 8 2020))
          (list "Exercise 2.2.9 - 2" (stamp 21 50 24 8 2020) (stamp 22 20 24 8 2020))
          (list "Exercise 2.2.9 - 2" (stamp 22 20 24 8 2020) (stamp 22 50 24 8 2020))
          (list "Exercise 2.2.9 - 2" (stamp 22 50 24 8 2020) (stamp 23 20 24 8 2020))
          (list "Exercise 2.2.9 - 2" (stamp 23 20 24 8 2020) (stamp 23 50 24 8 2020))
          (list "Exercise 2.2.9 - 2" (stamp 2 10 25 8 2020) (stamp 2 40 25 8 2020))
          (list "Exercise 2.2.9 - 2" (stamp 3 08 25 8 2020) (stamp 3 38 25 8 2020))
          (list "Exercise 2.2.9 - 2" (stamp 4 18 25 8 2020) (stamp 4 48 25 8 2020))
          (list "Exercise 2.2.9 - 2" (stamp 4 50 25 8 2020) (stamp 5 20 25 8 2020))
          (list "Exercise 2.2.9 - 2" (stamp 20 34 25 8 2020) (stamp 21 04 25 8 2020))
          (list "Exercise 2.2.9 - 2" (stamp 21 07 25 8 2020) (stamp 21 37 25 8 2020))
          (list "Exercise 2.2.9 - 2" (stamp 21 37 25 8 2020) (stamp 21 51 25 8 2020))
          (list "Exercise 2.2.9 - 3" (stamp 21 51 25 8 2020) (stamp 22 07 25 8 2020))
          (list "Exercise 2.2.9 - 4" (stamp 22 08 25 8 2020) (stamp 22 38 25 8 2020))
          (list "Exercise 2.2.9 - 4" (stamp 23 08 25 8 2020) (stamp 23 38 25 8 2020))
          (list "Exercise 2.2.9 - 5" (stamp 3 14 26 8 2020) (stamp 3 44 26 8 2020))
          (list "Exercise 2.2.9 - 5,6" (stamp 22 09 26 8 2020) (stamp 22 21 26 8 2020))
          (list "Section 2.3" (stamp 22 21 26 8 2020) (stamp 22 39 26 8 2020) 56)
          (list "Section 2.3" (stamp 00 58 26 8 2020) (stamp 1 11 26 8 2020) 58)
          (list "Exercise 2.3.1" (stamp 1 11 26 8 2020) (stamp 1 28 26 8 2020))
          (list "Exercise 2.3.1" (stamp 3 25 27 8 2020) (stamp 3 25 27 8 2020))
          (list "Exercise 2.3.1" (stamp 5 58 27 8 2020) (stamp 6 28 27 8 2020))
          (list "Exercise 2.3.1" (stamp 22 04 27 8 2020) (stamp 22 34 27 8 2020))
          (list "Exercise 2.3.2" (stamp 22 34 27 8 2020) (stamp 23 04 27 8 2020))
          (list "Exercise 2.3.3" (stamp 23 36 27 8 2020) (stamp 23 40 27 8 2020))
          (list "Exercise 2.3.4" (stamp 23 40 27 8 2020) (stamp 23 41 27 8 2020))
          (list "Exercise 2.3.5" (stamp 23 41 27 8 2020) (stamp 00 06 28 8 2020))
          (list "Exercise 2.3.6" (stamp 00 47 28 8 2020) (stamp 01 11 28 8 2020))
          (list "Exercise 2.3.7" (stamp 01 11 28 8 2020) (stamp 01 17 28 8 2020))
          (list "Section 2.3.2" (stamp 21 39 28 8 2020) (stamp 22 04 28 8 2020) 60)
          (list "Exercise 2.3.8" (stamp 22 04 28 8 2020) (stamp 22 09 28 8 2020))
          (list "Exercise 2.3.9" (stamp 22 09 28 8 2020) (stamp 22 19 28 8 2020))
          (list "Section 2.3.2" (stamp 22 19 28 8 2020) (stamp 22 24 28 8 2020) 62)
          (list "Exercise 2.3.10" (stamp 22 24 28 8 2020) (stamp 22 39 28 8 2020))
          (list "Exercise 2.3.10" (stamp 23 23 28 8 2020) (stamp 23 23 28 8 2020))
          (list "Exercise 2.3.10" (stamp 23 55 28 8 2020) (stamp 00 25 29 8 2020))
          (list "Exercise 2.2.9 - 2" (stamp 23 39 29 8 2020) (stamp 00 09 30 8 2020))
          (list "Exercise 2.2.9 - 2" (stamp 00 18 30 8 2020) (stamp 00 48 30 8 2020))
          (list "Exercise 2.3.1" (stamp 2 35 30 8 2020) (stamp 3 05 30 8 2020))
          (list "Exercise 2.3.10" (stamp 4 03 30 8 2020) (stamp 4 33 30 8 2020))
          (list "Exercise 2.3.10" (stamp 6 55 31 8 2020) (stamp 7 25 31 8 2020))
          (list "Exercise 2.3.10" (stamp 10 09 31 8 2020) (stamp 10 39 31 8 2020))
          (list "Execrise 2.3.10" (stamp 11 40 31 8 2020) (stamp 12 10 31 8 2020))
          (list "Exercise 2.3.10" (stamp 12 40 31 8 2020) (stamp 13 10 31 8 2020))
          (list "Exercise 2.3.10" (stamp 5 47 1 9 2020) (stamp 6 17 1 9 2020))
          (list "Exercise 2.3.10" (stamp 10 01 1 9 2020) (stamp 10 31 1 9 2020))
          (list "Exercise 2.3.10" (stamp 10 57 1 9 2020) (stamp 11 27 1 9 2020))
          (list "Exercise 2.3.10" (stamp 11 48 1 9 2020) (stamp 12 18 1 9 2020))
          (list "Exercise 2.2.9 - 4" (stamp 14 21 2 9 2020) (stamp 14 51 2 9 2020))
          (list "Exercise 2.3.10" (stamp 10 34 4 9 2020) (stamp 11 04 4 9 2020))
          (list "Exercise 2.3.10" (stamp 11 24 4 9 2020) (stamp 11 54 4 9 2020))
          (list "Exercise 2.3.10" (stamp 12 51 4 9 2020) (stamp 13 21 4 9 2020))
          (list "Exercise 2.3.10" (stamp 13 35 4 9 2020) (stamp 14 05 4 9 2020))
          (list "Exercise 2.3.10" (stamp 15 47 4 9 2020) (stamp 16 17 4 9 2020))
          (list "Exercise 2.3.11-13" (stamp 16 54 4 9 2020) (stamp 17 24 4 9 2020))
          (list "Exercise 2.3.10" (stamp 2 13 6 9 2020) (stamp 2 43 6 9 2020))
          (list "Exercise 2.3.10" (stamp 4 42 6 9 2020) (stamp 5 12 6 9 2020))
          (list "Exercise 2.3.10" (stamp 5 27 6 9 2020) (stamp 5 57 6 9 2020))
          (list "Exercise 2.3.10" (stamp 6 12 6 9 2020) (stamp 6 42 6 9 2020))
          (list "Exercise 2.3.10" (stamp 1 06 8 9 2020) (stamp 1 36 8 9 2020))
          (list "Exercise 2.3.10" (stamp 3 58 8 9 2020) (stamp 4 28 8 9 2020))
          (list "Exercise 2.3.10" (stamp 8 17 9 9 2020) (stamp 8 47 9 9 2020))
          (list "Exercise 2.3.10" (stamp 9 11 9 9 2020) (stamp 9 41 9 9 2020))
          (list "Exercise 2.3.10" (stamp 9 41 9 9 2020) (stamp 10 11 9 9 2020))
          (list "Exercise 2.3.10" (stamp 12 17 9 9 2020) (stamp 12 47 9 9 2020))
          (list "Section 2.3.3" (stamp 13 33 9 9 2020) (stamp 13 39 9 9 2020) 64)
          (list "Exercise 2.3.14" (stamp 13 39 9 9 2020) (stamp 14 03 9 9 2020))
          (list "Exercise 2.3.14" (stamp 14 19 9 9 2020) (stamp 14 49 9 9 2020))
          (list "Exercise 2.3.14" (stamp 10 13 10 9 2020) (stamp 10 43 10 9 2020))
          (list "Exercise 2.3.15" (stamp 11 09 10 9 2020) (stamp 11 16 10 9 2020))
          (list "Section 2.4" (stamp 11 16 10 9 2020) (stamp 11 28 10 9 2020) 66)
          (list "Section 3.1" (stamp 11 28 10 9 2020) (stamp 11 40 10 9 2020) 68)
          ))
   
   (list "Extras"
         (list (list "Reading List" (stamp 21 12 18 8 2020) (stamp 21 42 18 8 2020))
         (list "Reading List" (stamp 23 21 18 8 2020) (stamp 23 51 18 8 2020))
         (list "Building Timetable Generator" (stamp 2 20 19 8 2020) (stamp 2 50 19 8 2020))
         (list "Building Timetable Generator" (stamp 2 50 19 8 2020) (stamp 3 20 19 8 2020))
         (list "Building Timetable Generator" (stamp 3 20 19 8 2020) (stamp 3 50 19 8 2020))
         (list "Building Timetable Generator" (stamp 3 50 19 8 2020) (stamp 4 20 19 8 2020))
         (list "Adding Weekly Breakdown" (stamp 21 46 22 8 2020) (stamp 22 16 22 8 2020))
         (list "Adding Weekly Breakdown" (stamp 22 28 22 8 2020) (stamp 22 58 22 8 2020))
         (list "Adding Weekly Breakdown" (stamp 23 06 22 8 2020) (stamp 23 36 22 8 2020))
         (list "Adding Weekly Breakdown" (stamp 00 00 23 8 2020) (stamp 00 30 23 8 2020))
         (list "Adding Breakdown by Each Topic" (stamp 22 46 26 8 2020) (stamp 23 16 26 8 2020))
         (list "Adding Breakdown by Each Topic" (stamp 23 17 26 8 2020) (stamp 23 47 26 8 2020))
   ))))

(define start-date (stamp 00 00 30 7 2020))
(define week-start-date (stamp 00 00 27 7 2020))
(define last-date (third (last (last (last (rest (drop-right dates 1)))))))
(define elapsed-days (quotient (span week-start-date last-date) (* 24 60 60)))

(define (gen-dates from days)
  (map (lambda (n) (seconds->date (+ (date->seconds from) (* n 24 60 60)))) (range 0 (+ elapsed-days 2))))

(define (partition lst size repeat)
  (cond ((null? lst) '())
      ((> size (length lst)) (list lst))
  (else (let-values (((part1 part2) (split-at lst size)))
    (cons (append part1 (if (>= (length part2) repeat) (take part2 repeat) '()))
          (partition part2 size repeat))))))

(define date-ranges (drop-right (partition (gen-dates week-start-date elapsed-days) 1 1) 1))

(define (in-range start end log)
              (let ((date (second log)))
                (and (< (date->seconds start) (date->seconds date)) (<= (date->seconds date) (date->seconds end)))))

(define (time-in-range logs range)
  (let ((start (first range))
        (end (second range)))
    (map (compose second parse-row)
         (filter (lambda (log) (in-range start end log)) logs))))

(define logged-dates (append-map (lambda (n) (first (rest n))) dates))

(define month-names '("January" "February" "March" "April" "May" "June" "July" "August" "September" "October" "November" "December"))

(define (format-date date)
  (~a (date-day date) " " (list-ref month-names (- (date-month date) 1)) " " (date-year date)))

(define (format-week-entry-row row)
  (let ((date (first row))
        (length (second row)))
  (~a "| " (date->string date) " | " (in-hours-and-minutes length) " |")))

(define (collect-values logs range)
  (list (first range) (foldl + 0 (time-in-range logs range))))

(define (find-timings logs ranges)
  (map (lambda (range) (collect-values logs range)) ranges))

(define week-partition (partition (find-timings logged-dates date-ranges) 7 0))

(define weekly-breakdown
(string-join (map (lambda (ranges index)
               (~a "| *Week " index "* | " 
               "*" (in-hours-and-minutes (foldl + 0 (map second ranges))) "* |\n"
               (string-join (map format-week-entry-row ranges) "\n"))) week-partition (range 1 (+ 1 (length week-partition)))) "\n"))

(define (upsert dict key valfn)
(if (equal? (dict-ref dict key (lambda () #f)) #f)
        (dict-set dict key (valfn 0))
        (dict-update dict key valfn)))

(define (build-date-dict next acc)
    (upsert acc (car next) (lambda (n) (+ n (apply span (take (cdr next) 2))))))



(define (breakdown-by-time l)
(string-join (map (lambda (n) (~a "| " (first n) " | " (second n) " |"))
                  (sort
                   (dict-map l (lambda (k v) (list k (in-hours-and-minutes (in-minutes v)))))
      (lambda (a b) (string<? (car a) (car b))))) "\n"))


(define sections (filter (lambda (n) (or (equal? (substring (first n) 0 (string-length "Section")) "Section")
                                         (equal? (substring (first n) 0 (string-length "Overview")) "Overview"))) logged-dates))
(define exercises (filter (lambda (n) (equal? (substring (first n) 0 (string-length "Exercise")) "Exercise")) logged-dates))
(define extras (filter (lambda (n) (not (or (or (equal? (substring (first n) 0 (string-length "Overview")) "Overview")
                                         (equal? (substring (first n) 0 (string-length "Section")) "Section"))
                                               (equal? (substring (first n) 0 (string-length "Exercise")) "Exercise")))) logged-dates))


(define (time-breakdown-table title lst)
  (let ((result (foldl build-date-dict #hash() lst)))
    (~a (string-join
         (list (~a "** " title)
        (~a "Time taken: "
        (in-hours-and-minutes (in-minutes (foldl + 0 (dict-map result (lambda (k v) v))))))
        (breakdown-by-time result)) "\n"))))

(define section-times (time-breakdown-table "Reading" sections))
(define exercise-times (time-breakdown-table "Exercises" exercises))
(define extra-times (time-breakdown-table "Extras" extras))

;; TODO:
;; Breakdown of reading speed by every 100 pages:
;; 100/463 pages read in n pomodoros
;; 200/463 pages read in m pomodoros
;; Reading speed
;; Breakdown of time spent for exercises (by chapter and by 50 pomodoros?)

(with-output-to-file "./timetable.org" (lambda () (display
                                                   (string-join
                                                    (list
                                                       (~a "* Essentials of Programming Languages\n")
                                                     (~a "*Start Date*: " (date->string start-date))
                                                          (~a "*Latest Work Date*: " (date->string last-date))
                                                          (~a "*Elapsed Time*: "
                                                              elapsed-days " days")
                                                          "** Time taken"
                                                          section-times
                                                          exercise-times
                                                          extra-times
                                                          "** Log"
                                                   (build-table dates)
                                                   "** Weekly Breakdown"
                                                   weekly-breakdown) "\n"))) #:exists 'replace)
