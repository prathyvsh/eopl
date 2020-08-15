;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname exercise-1.2.2) (read-case-sensitive #t) (teachpacks ((lib "draw.rkt" "teachpack" "htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "draw.rkt" "teachpack" "htdp")) #f)))
#lang scheme

(define x1 '(a b))

(define x2 '(a))

(define x3 (cons x1 x2))

(equal? x3 '((a b) a))

(eq? (eq? x3 (cons x1 x2)) #f)

(eq? (eq? (cdr x3) x2) #t)

(eq? (eq? (car x1) (car x2)) #t)
;; Not defined

(equal? (cons (cons 'a 'b) (cons 'c '())) '((a . b) (c)))