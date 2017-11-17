
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (cond [(> low high) null]
        [(> (+ low stride) high) (cons low null)]
        [(cons low (sequence (+ low stride) high stride))]))

(define (string-append-map xs suffix)