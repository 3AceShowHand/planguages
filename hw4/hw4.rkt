
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (cond [(> low high) null]
        [(> (+ low stride) high) (cons low null)]
        [(cons low (sequence (+ low stride) high stride))]))

(define (string-append-map xs suffix)
  (map (lambda (x) (string-append x suffix))
       xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [(let ([ith (remainder n (length xs))])
           (car (list-tail xs ith)))]))

(define (stream-for-n-steps s n)
  (if (<= n 0)
      null
      (let ([pr (s)])
        (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1))))))

(define funny-number-stream
  (letrec ([f (lambda (x)
                (let ([t (if (= 0 (remainder x 5))
                             (- x)
                             x)])
                  (cons t (lambda() (f (+ x 1))))))])
    (lambda() (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x)
                (let ([t (if (odd? x) "dan.jpg" "dog.jpg")])
                  (cons t (lambda() (f (+ x 1))))))])
    (lambda() (f 1))))

(define (stream-add-zero s)
  (letrec ([f (lambda (stream)
                  (let ([t (stream)])
                    (cons (cons 0 (car t)) (lambda() (f (cdr t))))))])
    (lambda() (f s))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (let ([pr (cons (list-nth-mod xs n) (list-nth-mod ys n))])
                  (cons pr (lambda() (f (+ n 1))))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (if (= n (vector-length vec))
                    #f
                    (let ([t (vector-ref vec n)])
                      (cond [(and (pair? t) (equal? (car t) v)) t]
                            [#t (f (+ n 1))]))))])
    (f 0)))

(define (vector-assoc2 v vec)
         (letrec  ([len (vector-length vec)]
                   [f (lambda(current)
                     (cond[(= current len) #f]
                          [(pair? (vector-ref vec current)) (if (equal? (car (vector-ref vec current)) v)
                                                                (vector-ref vec current)
                                                                (f (+ current 1)))]
                          [#t (f (+ current 1))]))])
         (f 0)))

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [slot 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (if new-ans
                            (begin
                              (vector-set! memo slot new-ans)
                              (set! slot (remainder (+ slot 1) n))
                              new-ans)
                            new-ans)))))])
    f))
                              
                        
           

                    
      
  
  
