#lang racket

(provide (all-defined-out))

(define x 3)
(define y (+ x 2))


(define cube1
  (lambda (x)
    (* x (* x x))))


(define cube2
  (lambda (x)
    (* x x x)))


(define (cube3 x)
  (* x x x))


(define (pow1 x y) ; x to the yth power(y must be nonnegative)
  (if (= y 0)
      1
      (* x (pow1 x (- y 1)))))


(define pow2
  (lambda (x)
    (lambda (y)
      (pow1 x y))))


(define three-to-the (pow2 3))


; Racket Lists

;(list e1 ... en) for building lists

; sum all the numbers in a list
(define (sum xs)
  (if (null? xs)
      0
      (+ (car xs) (sum (cdr xs)))))

; append
(define (my-append xs ys)
  (if (null? xs)
      ys
      (cons (car xs) (my-append (cdr xs) ys))))

; map
(define (my-map f xs)
  (if (null? xs)
      null
      (cons (f (car xs))
            (my-map f (cdr xs)))))

(define foo (my-map (lambda (x) (+ x 1))
                    (cons 3 (cons 4 (cons 5 null)))))



















   