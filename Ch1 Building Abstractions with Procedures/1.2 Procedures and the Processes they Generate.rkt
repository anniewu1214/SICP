#lang planet neil/sicp

; e.g. linear recursion and iteration
(define (factorial_re n)
  (if (= n 1)
      1
      (* n (factorial_re (- n 1)))))

(define (factorial_it n) ; tail-recursion
  (define (factorial_ac n accumulator)
    (if (= n 0)
      accumulator
      (factorial_ac (- n 1) (* accumulator n))))
  (factorial_ac n 1))

(factorial_re 5)
(factorial_it 5)

; e.g. Tree Recursion
(define (fib_re n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib_re (- n 1)) (fib_re (- n 2))))))

(define (fib_it n) ; tail-recursion
  (define (fib_ac n a b)
    (if (= n 0)
      b
      (fib_ac (- n 1) (+ a b) a)))
  (fib_ac n 1 0))

(fib_re 5)
(fib_it 5)

; e.g. Counting Change: How many ways can we make change of $1.00,
; given half-dollars, quarters, dimes, nickels, and pennies? 
(define (count-change amount)
  ; ways to change amount a using n kinds of coins
  (define (cc amount kinds-of-coins) 
    (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount (- kinds-of-coins 1))
                 (cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))))

  ; the denomination of the first kind of coin
  (define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

  (cc amount 5))

(count-change 100)

; ex 1.9
; the first process is recursive and the second one iterative
(define (+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))
(define (+ a b)
  (if (= a 0) b (+ (dec a) (inc b))))

; ex 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(define (f n) (A 0 n)) ; f = 2n
(define (g n) (A 1 n)) ; g = 2^n
(define (h n) (A 2 n)) ; h = 2^2^...^2 (n times)

; ex 1.11
(define (f_re n)
  (if (< n 3)
      n
      (+ (f_re (- n 1)) (* 2 (f_re (- n 2))) (* 3 (f_re (- n 3))))))

(define (f_it n)
  (define (f_acc n a b c)
  (if (= n 0)
      c
      (f_acc (- n 1) (+ a (* 2 b) (* 3 c)) a b)))
  
  (f_acc n 2 1 0))

(f_re 5)
(f_it 5)