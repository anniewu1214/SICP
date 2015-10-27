#lang planet neil/sicp

; e.g. series summation
(define (sum f a next b)
  (if (> a b)
      0
      (+ (f a) (sum f (next a) next b))))

(define (average a b) (/ (+ a b) 2.0))
(define (average3 a b c) (/ (+ a b c) 3.0))
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
(define (accumulate-it combiner null-value f a next b)
  (define (iter a result)
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
(define (ab a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (prod-relative-prime n)
  (define (filter k) (= (gcd n k) 1))
  (filtered-accumulate * 1 identity 1 inc n filter))

(prod-relative-prime 5)


; ex 1.34
(define (f g) (g 2))
(f square) ; 4
(f (lambda (z) (* z (+ z 1)))) ; 6
(f f) ; -> (f 2) -> (2 2) -> error

; e.g half-interval method for finding routs of equations
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (< (abs (- neg-point pos-point)) 0.001) ; absolute convergence tolerance
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((> test-value 0) (search f neg-point midpoint))
                ((< test-value 0) (search f midpoint pos-point))
                (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (< a-value 0) (> b-value 0)) (search f a b))
          ((and (< b-value 0) (> a-value 0)) (search f b a))
          (else (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)
(half-interval-method (lambda (x) (- (cube x) (* 2 x) 3)) 1.0 2.0)

; e.g. finding fixed points of functions
(define (fixed-point f guess)
  (let ((next-guess (f guess)))
    (if (< (abs (- guess next-guess)) 0.001)
        next-guess
        (fixed-point f next-guess))))

(fixed-point cos 1.0)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

; average damping
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(sqrt 4)
(cube-root 9)

; ex 1.35
(fixed-point (lambda (x) (+ 1 (/ 1.0 x))) 1) ; 1.618

; ex 1.36
(define (fixed-point-print f guess)
  (display guess)
  (newline)
  (let ((next-guess (f guess)))
    (if (< (abs (- guess next-guess)) 0.001)
        next-guess
        (fixed-point-print f next-guess))))

(fixed-point-print (lambda (x) (/ (log 1000) (log x))) 10.0) ; 4.556

; ex 1.37 infinite continued fraction
(define (cont-frac n d k) ; n, d are procedures of one arg (index i) that return N_i and D_i
  (define (iter K)
    (if (> K k)
        0
        (/ (n K) (+ (d K) (iter (inc K))))))
  (iter 1))

(define (cont-frac-it n d k)
  (define (iter k result)
    (if (= k 0)
        result
        (iter (dec k) (/ (n k) (+ (d k) result)))))
  (iter k 0.0))

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 8)
(cont-frac-it (lambda (i) 1.0) (lambda (i) 1.0) 8)

; ex 1.38, e approximation
(+ 2 (cont-frac-it (lambda (i) 1.0)
              (lambda (i)
                (if (= (remainder (inc i) 3) 0)
                    (/ (* (inc i) 2) 3)
                    1))
              10))

; ex 1.39, tangent approximation
(define (tan-cf x k)
  (cont-frac-it
   (lambda (i) (if (= i 1) x (- (square x))))
   (lambda (i) (dec (* 2 i)))
   k))

; e.g. Newton's method
(define (newtons-method g guess)
  (define (deriv g)
    (let ((dx 0.01))
      (lambda (x) (/ (- (g (+ x dx)) (g x)) dx))))
  
  (fixed-point
   (lambda (x) (- x (/ (g x) ((deriv g) x))))
  guess))

(define (square-newton x)
  (newtons-method (lambda (y) (- (square y) x)) 1.0))

(square-newton 4)

; ex 1.40
(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x)) (* b x) c)))

(newtons-method (cubic 1 1 1) 1)

; ex 1.41
(define (double f)
  (lambda (x) (f (f x))))

((double inc) 1) ; increment by 2
(((double (double double)) inc) 5) ; increment by 16

; ex 1.42, f(g(x))
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

; ex 1.43, f(f(...f(x))), n times
; iterative version
(define (repeated f n)
  (define (iter n result)
    (if (= n 0)
        result
        (iter (dec n) (compose f result))))
  (iter n identity))

; recursive version
(define (repeated-rec f n)
  (if (= n 1)
      f
      (compose f (repeated-rec f (dec n)))))

((repeated square 2) 5)

; ex 1.44, n-fold smoothed function
(define (smooth f)
  (let ((dx 0.01))
    (lambda (x) (average3 (f (- x dx)) (f x) (f (+ x dx))))))

(define (n-fold-smooth f n)
  (repeated smooth n) f)

; ex 1.45, computing n-th roots
(define (pow b n) ; b^n
  (define (gather b n acc)
    (cond ((= n 0) acc)
          ((even? n) (gather (square b) (/ n 2) acc))
          (else (gather b (- n 1) (* acc b)))))
  
  (gather b n 1))

(define (log2 x) (/ (log x) (log 2)))

(define (nth-root x n)
  (fixed-point ((repeated average-damp (floor (log2 n))) (lambda (y) (/ x (pow y (- n 1))))) 
               1.0))

(nth-root 32 5)

; ex 1.46, iterative improvement
(define (iterative-improve good-enough? improve)
  (lambda (x)
    (let ((next (improve x)))
      (if (good-enough? x next)
          next
          ((iterative-improve good-enough? improve) next)))))

; absolute tolerance
(define (good-enough? x1 x2) 
  (let ((tol 0.001))
    (< (abs (- x1 x2)) tol)))

; rewrite sqrt using iterative-improve
(define (sqrt-new x init)
  ((iterative-improve good-enough?
                     (lambda (y) (average (/ x y) y))) init)

; rewrite fixed-point using iterative-improve
(define (fixed-point-new f init)
  ((iterative-improve good-enough? f) init))