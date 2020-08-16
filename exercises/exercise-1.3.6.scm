#lang scheme

;; The advantage of automatic currying is that
;; you can swiftly create curried versions of
;; functions which can be composed together with
;; multiple versions of the function.

;; The disadvantage is that error messages wouldn't
;; be generated when you pass fewer than requisite
;; arguments to a function. Automatic currying would
;; mean that an incorrect application would mean a
;; correct partial one.