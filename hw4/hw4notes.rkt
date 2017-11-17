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

; Parentheses matter!
(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

; Dynamic Typing
(define xs (list 4 5 6))
(define ys (list (list 4 5) 6 7 (list 8) 9 2 3 (list 0 1)))
(define zs (list #f "hi" 14))

(define (sum1 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum1 (cdr xs)))
          (+ (sum1 (car xs)) (sum1 (cdr xs))))))

; this version ignore any element not a number
(define (sum2 xs)
  (if (null? xs)
      0
      (if (number? (car xs))
          (+ (car xs) (sum1 (cdr xs)))
          (if (list? (car xs))
              (+ (sum2 (car xs)) (sum2 (cdr xs)))
              (sum2 (cdr xs))))))

; Cond
; For "if" and "Cond", treat anything other than #f as true.
(define (sum3 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum3 (cdr xs)))]
        [#t (+ (sum3 (car xs)) (sum3 (cdr xs)))]))

(define (sum4 xs)
  (cond [(null? xs) 0]
        [(number? (car xs)) (+ (car xs) (sum4 (cdr xs)))]
        [(list? (car xs)) (+ (sum4 (car xs)) (sum4 (cdr xs)))]
        [(sum4 (cdr xs))]))

(define (count-false xs)
  (cond [(null? xs) 0]
        [(car xs) (count-false (cdr xs))]  ;(car xs) can have anything, and then evaluate to true
        [(+ 1 (count-false (cdr xs)))]))

(define (max-of-list xs)
  (cond [(null? xs) (error "max-of-list given empty list")]
        [(null? (cdr xs) (car xs))]
        [(let ([tlans (max-of-list (cdr xs))])
           (if (> tlans (car xs))
               tlans
               (car xs)))]))

; Let: can bind any number of local variables
(define (silly-double x)
  (let ([x (+ x 3)]   ; x refer to the first x
        [y (+ x 2)])  ; x also refer to the first x
    (+ x y - 5)))

; Let*: the expression are evaluated in the environment produced from the previous bindings
(define (silly-double1 x)
  (let* ([x (+ x 3)]
         [y (+ x 2)])
    (+ x y -8)))

; Letrec: the expressions are evaluated in the environment that includes all the bindings
; Needed for mutual recursion
; Remember function bodies not evaluated until called
(define (silly-triple x)
  (letrec ([y (+ x 2)]
           [f (lambda (z) (+ z y w x))]
           [w (+ x 7)])
    (f -9)))

; define has the same semantics as letrec
(define (silly-mod x)
  (define (even? x) (if (zero? x) #t (odd? (- x 1))))
  (define (odd? x) (if (zero? x) #f (even? (- x 1))))
  (if (even? x) 0 1))

; Top-level
; Unlike ML, Racket can refer to later bindings
; But refer to later bindings only in function bodies
; Because bindings are evaluated in order
(define (f x) ( + x (* x b))) ;this is ok to refer b in later bindings ,because function is not called yet.
(define b 3)
(define c (+ b 4))

;(define d (+ e 4)) ;not okay (get an error)
(define e 5)
;(define f 17) ; f is alread defined in this module

; Mutation with set!

; Environment for closure determined when function is defined,
; but body is evaluted when function is called.
(define f1 (lambda (x) (* 1(+ x b))))
(set! b 5)
(define z (f1 4))    ;9  now b=5
(define w c)        ;7

; If something you need not to change might change, make a local copy of it.
(define f2
  (let ([b b])        ;first b is a copy of second b, could use a different name but not need to.
    (lambda (x) (* 1(+ x b))))) ;this b refer to the first b above line.

; The Truth About Cons
(define pr (cons 1 (cons #t "hi"))) ;This is not a list, but a pair
(define lst (cons 1 (cons #t (cons "hi" null)))) ;this is a list
(define hi (cdr (cdr pr)))

; list? return true for proper lists, including the empty list
; pair? return true for things made by cons

; cons cells are immutable
; it made aliasing irrelavant

; set! does not mutate cons cells.

; mcons cells are mutable
; mcons
; mcar
; mcdr
; mpair?
; set-mcar!
; set-mcdr!
(define mpr (mcons 1 (mcons #t "hi")))
(mcar mpr)
(mcdr mpr)
(set-mcdr! mpr 47)

; Delayed Evaluation and Thunks
; When expressions get evaluated ?

; Delay evaluation: put things we don't want yet in a function
; A zero-argument function used to delay evaluation is called a thunk
(define (my-if x y z)
  (if x (y) (z)))

(define (fact2 n)
  (my-if (= n 0)
         (lambda() 1)
         (lambda() (* (fact2 (- n 1))))))

; Avoid unnecessary Computations
(define (slow-add x y)
  (letrec ([slow-id (lambda (y z)
                      (if (= z 0)
                          y
                          (slow-id y (- z 1))))])
    (+ (slow-id x 50000000) y)))

; multiplies x and result of y-thunk, calling y-thunk x times
(define (my-mult x y-thunk)
  (cond [(= x 0) 0]
        [(= x 1) (y-thunk)]
        [(+ (y-thunk) (my-mult (- x 1) y-thunk))]))

; Assuming some expensive computation has no side effects, ideally
; we would:
; - Not compute it until needed.
; - Remember the answer so future uses complete immediately Called lazy evaluation

; Delay and Force
(define (my-delay th)
  (mcons #f th))

(define (my-force p)
  (if (mcar p)
      (mcdr p)
      (begin (set-mcar! p #t)
             (set-mcdr! p ((mcdr p)))
             (mcdr p))))

(my-mult 100 (let ([p (my-delay (lambda() (slow-add 3 4)))])
              (lambda() (my-force p))))

; Using Streams
; A stream is an infinite sequence of values, but use a thunk to delay creating most
; of the sequence

; Stream producer knows how to create any number of values
; Stream consumer decides how many values to ask for

; Let a stream be a thunk that when called returns a pair
; '(next-answer . next-thunk) 

; 1 1 1 1 1 1 1
(define ones (lambda() (cons 1 ones)))

(define (f3 x) (cons x (lambda () (f (+ x 1)))))
(define nats
  (letrec ([f (lambda (x) (cons x (lambda() (f (+ x 1)))))])
    (lambda() (f 1))))

; 2 4 8 16 ...
(define powers-of-two
  (letrec ([f (lambda (x) (cons x (lambda() (f (* x 2)))))])
    (lambda() (f 1))))

(define (number-until stream tester)
  (letrec ([f (lambda (stream ans)
                (let ([pr (stream)])
                  (if (tester (car pr))
                      ans
                      (f (cdr pr) (+ ans 1)))))])
    (f stream 1)))

(number-until powers-of-two (lambda(x) (= x 16)))

; Memoization
; If a function has no side effects and does not read mutable memory,
; no point in computing it twice for the same arguments
; Can keep a cache of previous results:
;    1. if mataining cache is cheaper than recomputing
;    2. cached results are reused

(define (fibonacci1 x)
  (if (or (= x 0) (= x 1))
      x
      (+ (fibonacci1 (- x 1))
         (fibonacci1 (- x 2)))))

(define (fibonacci2 x)
  (letrec ([f (lambda (acc1 acc2 y)
                (if (= y x)
                    (+ acc1 acc2)
                    (f (+ acc1 acc2) acc1 (+ y 1))))])
    (if (or (= x 0) (= x 1))
        x
        (f 0 1 1))))

(define fibonacci3
  (letrec ([memo null]     ; list of pairs (arg . result)
           [f (lambda (x)
                (let ([ans (assoc x memo)])   ; check if x in memo, return the element x in, else false.
                  (if ans
                      (cdr ans)
                      (let ([new-ans (if (or (= x 0) (= x 1))
                                             x
                                             (+ (f (- x 1))
                                                (f (- x 2))))])
                        (begin
                          (set! memo (cons (cons x new-ans) memo))
                          new-ans)))))])
    f))

; Macro: The Key Points
; A macro is one way to implement syntactic sugar
; Paying attention to the order expressions evaluate and how many times.

; Hygienic macros
; Secretly renames local variables in macros with fresh names
; Looks ip variables used in macros where the macro is defined













