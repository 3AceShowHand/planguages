
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (cond [(> low high) null]
        [(> (+ low stride) high) (cons low null)]
        [(cons low (sequence (+ low stride) high stride))]))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append suffix x))
       xs))

;(define (list-nth-mod xs n))

;(define (stream-for-n-steps s n))

;(define (funny-number-stream))

;(define (dan-then-dog))

;(define (stream-add-zero))

;(define (cycle-lists xs ys))

;(define (vector-assoc v vec))

;(define (cached-assoc xs n))