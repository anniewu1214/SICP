#lang planet neil/sicp

; delay the evaluation of a procedure
(define-syntax delay
  (syntax-rules ()
    ((delay proc)
     (memo-proc (lambda () proc)))))

(define (memo-proc proc)
  (let ((already-run? #f) (result #f))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? #t)
                 result)
          result))))

; force the evaluation of a delayed procedure
(define (force delayed-proc) (delayed-proc))

; stream primitives
(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (delay b)))))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (dec n))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each (lambda (x) (newline) (display x)) s))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream low
                   (stream-enumerate-interval (inc low) high))))

(define (square x) (* x x))

(define (prime? n)
  (define (smallest-divisor n test)
    (cond ((> (square test) n) n)
          ((= (remainder n test) 0) test)
          (else (smallest-divisor n (+ test 1)))))
  (if (= n 1)
      #f
      (= (smallest-divisor n 2) n)))

(stream-car
 (stream-cdr
  (stream-filter prime?
                 (stream-enumerate-interval 10000 1000000)))) ; 10009

; ex 3.50
(define (stream-map-general proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map-general
              (cons proc (map stream-cdr argstreams))))))

; ex 3.51
(define (display-line x) (display x) (newline))
(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10))) ; 0
(stream-ref x 5) ; 1 2 3 4 5
(stream-ref x 7) ; 6 7

; ex 3.52
(define sum 0)
(define (accum x) (set! sum (+ sum x)) sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))

(display-line sum) ; 1

(define y (stream-filter even? seq)) ; 

(display-line sum) ; 6
(display seq)

(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))

(display-line sum) ; 10

(stream-ref y 7) ; 136
(display-line sum) ; 136

(display-stream z) ; 10 15 45 55 105 120 190 210
(display-line sum) ; 210
; the results will be different because seq is evaluated twice in y and z

; integer streams
(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (inc n))))
(define integers (integers-starting-from 1))

; stream of ints not divisible by 7
(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7))) integers))

(stream-ref no-sevens 100) ; 117

; streams of Fibonacci
(define (fibgen a b) (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

; sieve of Eratosthenes
(define (sieve stream)
  (cons-stream (stream-car stream)
               (sieve (stream-filter
                       (lambda (x)
                         (not (divisible? x (stream-car stream))))
                       (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))
(stream-ref primes 50)

; defining streams implicitly
(define ones (cons-stream 1 ones))
(define (add-streams s1 s2) (stream-map-general + s1 s2))
(define integers-2 (cons-stream 1 (add-streams ones integers)))

; ex 3.57
; nth fib is the sum of two previous fibs. if fibs are memorized,
; f(n) = f(n-1) + f(n-2), one additional add is needed, and fib is
; linear; if not, we have to recompute f(n-1) and f(n-2), fib is exponential.
(define fibs-2
  (cons-stream
   0
   (cons-stream 1 (add-streams (stream-cdr fibs-2) fibs-2))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define double (cons-stream 1 (scale-stream double 2)))

; ***
(define primes-2
  (cons-stream
   2
   (stream-filter prime?-2 (integers-starting-from 3))))

(define (prime?-2 n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)
          (else (iter (stream-cdr ps)))))
  (iter primes-2))

(stream-ref primes-2 10)

; ex 3.53
(define s (cons-stream 1 (add-streams s s))) ; stream of 2n

; ex 3.54, multiply streams
(define (mul-streams s1 s2) (stream-map-general * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams (integers-starting-from 2) factorials)))

(stream-ref factorials 4)

; ex 3.55, S0, S0 + S1, S0 + S1 + S2
(define (partial-sums S)
  (cons-stream (stream-car S)
               (add-streams (stream-cdr S)
                            (partial-sums S))))

(stream-ref (partial-sums integers) 4) ; 15

; ex 3.56, merge streams
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream
                   s2car
                   (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream
                   s1car
                   (merge (stream-cdr s1)
                          (stream-cdr s2)))))))))

(define SS (cons-stream 1 (merge (scale-stream S 2)
                                 (merge
                                  (scale-stream S 3)
                                  (scale-stream S 5)))))

; print the first n elemnts of a stream
(define (display-first-terms stream n)
  (if (> n 0)
      (begin (display (stream-car stream))
             (display " ")
             (display-first-terms (stream-cdr stream) (dec n)))))

(display-first-terms SS 10) (newline)

; ex 3.58, [num/den] in radix
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(display-first-terms (expand 1 7 10) 10) (newline)
(display-first-terms (expand 3 8 10) 10) (newline)


; ex 3.59, power series
(define (integrate-series coeff-stream)
  (stream-map-general * (stream-map-general / ones integers) coeff-stream))

(define exp-series (cons-stream 1 (integrate-series exp-series)))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(display-first-terms exp-series 5) (newline) ; 1 1 1/2 1/6 1/24
(display-first-terms (integrate-series integers) 5) (newline) ; 1 1 1 1 1
(display-first-terms cosine-series 5) (newline) ; 1 0 -1/2 0 1/24 
(display-first-terms sine-series 5) (newline) ; 0 1 0 -1/6 0

; ex 3.60, multiply series
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams
                (scale-stream (stream-cdr s2) (stream-car s1))
                (mul-series (stream-cdr s1) s2))))

(define sinx^2+cosx^2
  (add-streams
   (mul-series sine-series sine-series)
   (mul-series cosine-series cosine-series)))

(display-first-terms sinx^2+cosx^2 5) (newline) ; 1 0 0 0 0

; ex 3.61, invert series
(define (invert-series s)
  (cons-stream 1
               (mul-series (scale-stream (stream-cdr s) -1)
                           (invert-series s))))

(define unit
  (mul-series cosine-series
              (invert-series cosine-series)))

(display-first-terms unit 5) (newline) ; 1 0 0 0 0

; ex 3.62, devide series
(define (div-series s1 s2)
  (let ((constant-denom (stream-car s2)))
    (if (= constant-denom 0)
        (error "denominator has zero constant term -- DIV-SERIES" s2)
        (mul-series s1
                    (scale-stream
                     (invert-series
                      (scale-stream s2
                                    (/ 1 constant-denom)))
                     (/ 1 constant-denom))))))

(define tangent-series
  (div-series sine-series cosine-series))

(display-first-terms tangent-series 7) (newline) ; 0 1 0 1/3 0 2/15 0


; Stream Paradigm: represent state as a timeless stream of values
; rather than a set of variables to be updated.

; implement sqrt using streams
(define (sqrt-improve guess x)
  (define (average a b) (/ (+ a b) 2.0))
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
     1.0
     (stream-map (lambda (guess) (sqrt-improve guess x))
                 guesses)))
  guesses)

(display-first-terms (sqrt-stream 2) 7) (newline)

; estimate pi using streams
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (scale-stream (pi-summands (+ n 2)) -1)))

(define pi-stream (scale-stream (partial-sums (pi-summands 1)) 4))
(display-first-terms pi-stream 7) (newline)

; sequence accelerator
(define (euler-transform s)
  (let ((s0 (stream-ref s 0)) ; S(n-1)
        (s1 (stream-ref s 1)) ; S(n)
        (s2 (stream-ref s 2))) ; S(n+1)
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(display-first-terms (euler-transform pi-stream) 7) (newline)

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(display-first-terms
 (accelerated-sequence euler-transform pi-stream) 7) (newline)

; ex 3.63
; without the local variable, the results won't be memorized, to compute
; the n-th term, we recompute the (n-1)th term, (n-2)th term, and so on.
(define (sqrt-stream-slow x)
  (cons-stream 1.0
               (stream-map (lambda (guess)
                             (sqrt-improve guess x))
                           (sqrt-stream-slow x))))

; ex 3.64
(define (stream-limit stream tolerance)
  (let ((first (stream-car stream))
        (second (stream-car (stream-cdr stream))))
    (if (> tolerance (abs (- first second)))
        second
        (stream-limit (stream-cdr stream) tolerance))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(sqrt 2.0 0.01)

; ex 3.65
(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (scale-stream (ln2-summands (inc n)) -1)))

(define ln2-stream (partial-sums (ln2-summands 1)))
(define accelerated-ln2 (accelerated-sequence euler-transform ln2-stream))

(display-first-terms accelerated-ln2 7) (newline)