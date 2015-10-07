#lang planet neil/sicp

; ex 1.3
(define (square-sum-largest-two x y z)
  (cond
    ((and (>= x y) (>= y z)) (+ (* x x) (* y y)))
    ((and (>= x y) (>= z y)) (+ (* x x) (* z z)))
    ((and (>= y x) (>= z x)) (+ (* z z) (* y y)))))

; ex 1.4
; if returns either - or + depending on b
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; ex 1.5
; for applicative-order eval: infinite loop; for normal-order eval: returns 0
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
(test 0 (p))

; ex 1.6
; new-if will call sqrt-iter infinitely, because of applicative-order evaluation
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

; ex 1.7
; absolute tolerance is not reasonable for small and large values, use relative tol instead
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (improve guess) guess)) (* guess 0.001)))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0 x))

(sqrt 0.000001)
(sqrt 10000000000)

; ex 1.8
(define (cube-root x)
  (define (good-enough? guess x)
    (< (abs (- (improve guess x) guess)) (abs (* guess 0.001))))
  (define (cube-root-iter guess x)
    (if (good-enough? guess x)
        guess
        (cube-root-iter (improve guess x) x)))
  (define (improve guess x)
    (/ (+ (/ x (* guess guess)) guess guess) 3))
  (cube-root-iter 1.0 x))

(cube-root 0.000001)
(cube-root 1000000000)