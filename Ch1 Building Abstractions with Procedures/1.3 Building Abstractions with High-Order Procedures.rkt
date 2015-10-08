#lang planet neil/sicp

; e.g. series summation
(define (sum f a next b)
  (if (> a b)
      0
      (+ (f a) (sum f (next a) next b))))


(define (square x) (* x x))
(define (inc x) (+ 1 x))
(define (cube x) (* x x x))
(define (identity x) x)

; cubic summation
(define (sum-cubes a b) (sum cube a inc b))
; integer summation
(define (sum-ints a b) (sum identity a inc b))
; pi approximation
(define (pi-sum a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (sum pi-term a pi-next b))
; integral approximation
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* dx (sum f (+ a (/ dx 2.0)) add-dx b)))

(sum-cubes 1 10)
(sum-ints 1 10)
(* 8 (pi-sum 1 1000))
(integral cube 0 1 0.01)

; ex 1.29, integration using Simpson's Rule
(define (simpson-integral f a b n)
   (define even-n (+ n (remainder n 2)))
   (define h (/ (- b a) even-n))
   (define (simpson-term k)
     (define y (f (+ a (* k h))))
     (if (or (= k 0) (= k even-n))
         (* 1 y)
         (if (even? k)
             (* 2 y)
             (* 4 y))))
  
   (* (/ h 3) (sum simpson-term 0 inc even-n)))

(simpson-integral cube 0 1 100)

; ex 1.30, iterative series summation
(define (sum-iter f a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (f a)))))
  (iter a 0))

; ex 1.31, series production
(define (product f a next b)
  (if (> a b)
      1
      (* (f a) (product f (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

(define (pi-prod a b)
  (define (f x)
    (if (even? x)
        (/ (+ x 2.0) (+ x 1))
        (/ (+ x 1.0) (+ x 2))))
  (product f 1 inc b))
  

(factorial 5)
(* 4 (pi-prod 1 100))

; iterative series production using an accumulator
(define (product-iter f a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (f a)))))
  (iter a 1))

; ex 1.32, generalization of summation and production
(define (accumulate combiner null-value f a next b)
  (if (> a b)
      null-value
      (combiner (f a) (accumulate combiner null-value f (next a) next b))))

(define (sum-der f a next b) (accumulate + 0 f a next b))

(define (product-der f a next b) (accumulate * 1 f a next b))

; iterative version
(define (accumulate combiner null-value f a next b)
  (define iter(a result)
    (if (> a b)
        result
        (iter (next a) (combiner (f a) result))))
  (iter a null-value))

; ex 1.33
(define (filtered-accumulate combiner null-value f a next b filter)
  (if (> a b) null-value
      (if (filter a)
          (combiner (f a) (filtered-accumulate combiner null-value f (next a) next b filter))
          (filtered-accumulate combiner null-value f (next a) next b filter))))

; square sum of primes numbers in [a, b]
(define (square-sum-primes a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (prime? n)
  (define (smallest-divisor n test)
    (cond ((> (square test) n) n)
        ((= (remainder n test) 0) test)
        (else (smallest-divisor n (+ test 1)))))
  
  (if (= n 1)
      #f
      (= (smallest-divisor n 2) n)))
  
(square-sum-primes 1 10)

; product of positive integers less than n that are relatively prime to n
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (prod-relative-prime n)
  (define (filter k) (= (gcd n k) 1))
  (filtered-accumulate * 1 identity 1 inc n filter))

(prod-relative-prime 5)
