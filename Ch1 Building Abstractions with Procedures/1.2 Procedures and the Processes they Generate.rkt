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

; ex 1.9, define addition using dec, inc
; the first process is recursive and the second one iterative
(define (1+ a b)
  (if (= a 0) b (inc (+ (dec a) b))))
(define (2+ a b)
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

; ex 1.12, pascal triangle
(define (pascal-triangle row col)
  (cond ((or (> col row) (< col 1) (< row 1)) 0)
        ((= col 1) 1)
        (else  (+ (pascal-triangle (- row 1) (- col 1))
                  (pascal-triangle (- row 1) col)))))

(pascal-triangle 1 1) 
(pascal-triangle 4 2)  
(pascal-triangle 5 3)

; ex 1.15, approximation of sin(x)
; time complexity = O(log(x))
(define (sine x)  
  (define (p x) (- (* 3 x) (* 4 (cube x))))
  
  (if (<= (abs x) 0.1)
      x
      (p (sine (/ x 3.0)))))

; ex 1.16
(define (expt-recursive b n)
  (if (= n 0)
      1
      (* b (expt-recursive b (- n 1)))))

(define (expt-accumulator b n)
  (define (gather b n acc)
    (if (= n 0)
        acc
        (gather b (- n 1) (* acc b))))
  
  (gather b n 1))

(define (even? n) (= (remainder n 2) 0))
(define (square x) (* x x))

(define (expt-fast b n)
  (cond ((= n 0) 1)
        ((even? n) (square (expt-fast b (/ n 2))))
        (else (* b (expt-fast b (- n 1))))))

(define (expt-faster-accmulator b n)
  (define (gather b n acc)
    (cond ((= n 0) acc)
          ((even? n) (gather (square b) (/ n 2) acc))
          (else (gather b (- n 1) (* acc b)))))
  
  (gather b n 1))


(expt-recursive 2 10)
(expt-accumulator 2 10)
(expt-fast 2 10) 
(expt-faster-accmulator 2 10) 

; ex 1.17, define multiplication using double and halve
(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (multiplication a b)
  (cond ((= b 0) 0)
        ((even? b) (double (multiplication a (halve b))))
        (else (+ a (multiplication a (- b 1))))))

(multiplication 2 10)

; ex 1.18
(define (multiplication-accumulator a b)
  (define (gather a b acc)
    (cond ((= b 0) acc)
          ((even? b) (gather (double a) (halve b) acc))
          (else (gather a (- b 1) (+ acc a)))))
  
  (gather a b 0))

(multiplication-accumulator 2 10)

; ex 1.19
; normal-order eval: 18 remainder operations
; applicative-order eval: 4 remainder operations
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; e.g. test primality
; brutal force search
(define (smallest-divisor n test)
  (cond ((> (square test) n) n)
        ((= (remainder n test) 0) test)
        (else (smallest-divisor n (+ test 1)))))

(define (brutal-prime? n) 
  (if (= n 1)
      #f
      (= (smallest-divisor n 2) n)))

(brutal-prime? 97)
 
; fermat test
(define (fast-prime? n times)
  (define (expmod a n m)
    (cond ((= n 0) 1)
          ((even? n) (remainder (square (expmod a (/ n 2) m)) m))
          (else (remainder (* a (expmod a (- n 1) m)) m))))
  
  (define (fermat-test n)
    (define (try-it a) (= (expmod a n n) a))
    (try-it (+ 1 (random (- n 1)))))
  
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(fast-prime? 97 100)

; ex 1.21
(smallest-divisor 199 2)
(smallest-divisor 1999 2)
(smallest-divisor 19999 2)

; ex 1.22
(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    (define (report-prime elapsed-time)
      (display " *** ")
      (display elapsed-time))
    
    (if (brutal-prime? n)
        (report-prime (- (runtime) start-time))))
  
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(timed-prime-test 1999)

(define (search-for-primes start end)
  (define (search-iter cur end)
    (if (<= cur end) (timed-prime-test cur))
    (if (<= cur end) (search-for-primes (+ cur 2) end)))
  
  (search-iter (if (even? start) (+ 1 start) start) end))

; time complexity of brutal-prime? is sqrt(n)
(search-for-primes 1000 1013) ; 3
(search-for-primes 10000 10013) ; 10 
(search-for-primes 100000 100013) ; 29
(search-for-primes 1000000 1000013) ; 91

; ex 1.23
(define (smallest-divisor n test)
  (define (next test)
    (if (= test 2) 3 (+ test 2)))
  
  (cond ((> (square test) n) n)
        ((= (remainder n test) 0) test)
        (else (smallest-divisor n (next test)))))

; 1.5* faster due to the overhead of IF test in the next procedure
(search-for-primes 1000 1013) ; 3 -> 2
(search-for-primes 10000 10013) ; 10 -> 7
(search-for-primes 100000 100013) ; 29 -> 21
(search-for-primes 1000000 1000013) ; 91 -> 61

; ex 1.24 
(define (timed-prime-test n)
  (define (start-prime-test n start-time)
    (define (report-prime elapsed-time)
      (display " *** ")
      (display elapsed-time))
    
    (if (fast-prime? n 100) ; do fermat test 100 times
        (report-prime (- (runtime) start-time))))
  
  (newline)
  (display n)
  (start-prime-test n (runtime)))

; logarithmic time complexity
(search-for-primes 1000 1013) ; 3 -> 2 -> 257
(search-for-primes 10000 10013) ; 10 -> 7 -> 342
(search-for-primes 100000 100013) ; 29 -> 21 -> 469
(search-for-primes 1000000 1000013) ; 91 -> 61 -> 452

; ex 1.25, this approach is far less effective than the precious one.
; ex 1.26, this results in a tree recursion, whose time complexity is
;          exponential to the tree depth lg(N) => linear time complexity.
