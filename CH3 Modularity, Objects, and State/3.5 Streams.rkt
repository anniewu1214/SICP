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

(define y (stream-filter even? seq))

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
  (define (iter stream n)
    (if (> n 0)
        (begin (display (stream-car stream))
               (display " ")
               (iter (stream-cdr stream) (dec n)))))
  (iter stream n)
  (newline))

(display-first-terms SS 10)

; ex 3.58, [num/den] in radix
(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(display-first-terms (expand 1 7 10) 10)
(display-first-terms (expand 3 8 10) 10)


; ex 3.59, power series
(define (integrate-series coeff-stream)
  (stream-map-general * (stream-map-general / ones integers) coeff-stream))

(define exp-series (cons-stream 1 (integrate-series exp-series)))
(define sine-series (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))

(display-first-terms exp-series 5) ; 1 1 1/2 1/6 1/24
(display-first-terms (integrate-series integers) 5) ; 1 1 1 1 1
(display-first-terms cosine-series 5) ; 1 0 -1/2 0 1/24 
(display-first-terms sine-series 5) ; 0 1 0 -1/6 0

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

(display-first-terms sinx^2+cosx^2 5) ; 1 0 0 0 0

; ex 3.61, invert series
(define (invert-series s)
  (cons-stream 1
               (mul-series (scale-stream (stream-cdr s) -1)
                           (invert-series s))))

(define unit
  (mul-series cosine-series
              (invert-series cosine-series)))

(display-first-terms unit 5) ; 1 0 0 0 0

; ex 3.62, divide series
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

(display-first-terms tangent-series 7) ; 0 1 0 1/3 0 2/15 0


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

(display-first-terms (sqrt-stream 2) 7)

; estimate pi using streams
(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (scale-stream (pi-summands (+ n 2)) -1)))

(define pi-stream (scale-stream (partial-sums (pi-summands 1)) 4))
(display-first-terms pi-stream 7)

; sequence accelerator
(define (euler-transform s)
  (let ((s0 (stream-ref s 0)) ; S(n-1)
        (s1 (stream-ref s 1)) ; S(n)
        (s2 (stream-ref s 2))) ; S(n+1)
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(display-first-terms (euler-transform pi-stream) 7)

(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(display-first-terms
 (accelerated-sequence euler-transform pi-stream) 7)

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

(display-first-terms accelerated-ln2 7)

; infinite streams of pairs
(define (interleave s1 s2)
  ;;; interleave two infinite streams
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define int-pairs (pairs integers integers))

(stream-filter (lambda (pair)
                 (prime? (+ (car pair) (cadr pair))))
               int-pairs)

; ex 3.66
; the number of preceding paris of (i,j) is 2^(i-1)(max(1, 2(j-i))+1) - 2
(display-first-terms int-pairs 10)

; ex 3.67
(define (all-pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list x (stream-car t)))
                (stream-cdr s))
    (interleave
     (stream-map (lambda (x) (list (stream-car s) x))
                 (stream-cdr t))
     (all-pairs (stream-cdr s) (stream-cdr t))))))

; ex 3.68
; bad-pairs will run into inifinite loop
(define (bad-pairs s t)
  (interleave
   (stream-map (lambda (x) (list (stream-car s) x))
               t)
   (bad-pairs (stream-cdr s) (stream-cdr t))))


; ex 3.69
(define (triples s t u)
  (cons-stream (list (stream-car s)
                     (stream-car t)
                     (stream-car u))
               (interleave
                (stream-map (lambda (x)
                              (cons (stream-car s) x))
                            (stream-cdr (pairs t u)))
                (triples (stream-cdr s)
                         (stream-cdr t)
                         (stream-cdr u)))))

(define int-triples (triples integers integers integers))

(define pythagorean-triples
  (stream-filter (lambda (x)
                   (= (+ (square (car x)) (square (cadr x)))
                      (square (caddr x))))
                 int-triples))

(display-first-terms pythagorean-triples 3) ; (3 4 5) (6 8 10) (5 12 13)

; ex 3.70, order pairs by weight
(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< (weight s1car) (weight s2car))
                  (cons-stream s1car
                               (merge-weighted (stream-cdr s1) s2 weight)))
                 ((> (weight s1car) (weight s2car))
                  (cons-stream s2car
                               (merge-weighted s1 (stream-cdr s2) weight)))
                 (else
                  (cons-stream s1car
                               (cons-stream s2car
                                            (merge-weighted (stream-cdr s1) (stream-cdr s2) weight)))))))))

(define (weighted-pairs s t weight)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (merge-weighted
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (weighted-pairs (stream-cdr s) (stream-cdr t) weight)
    weight)))

(define pairs-ordered-by-sum
  (weighted-pairs integers
                  integers
                  (lambda (x) (+ (car x) (cadr x)))))

(display-first-terms pairs-ordered-by-sum 5) ; (1 1) (1 2) (1 3) (2 2) (1 4)

(define pairs-ordered-by-blabla
  (let ((ints-blabla
         (stream-filter (lambda (x) (not (or (divisible? x 2)
                                             (divisible? x 3)
                                             (divisible? x 5))))
                        integers)))
    (weighted-pairs ints-blabla
                    ints-blabla
                    (lambda (x)
                      (let ((i (car x))
                            (j (cadr x)))
                        (+ (* 2 i) (* 3 j) (* 5 i j)))))))

(display-first-terms pairs-ordered-by-blabla 5) ; (1 1) (1 7) (1 11) (1 13) (1 17)

; ex 3.71, Ramanujan numbers
(define (cube x) (* x x x))

(define (cube-sum-weight x)
  (+ (cube (car x))
     (cube (cadr x))))

(define pairs-ordered-by-cube-sum
  (weighted-pairs integers
                  integers
                  cube-sum-weight))

(display-first-terms pairs-ordered-by-cube-sum 5)

(define Ramanujan-numbers
  (let ((weights (stream-map cube-sum-weight
                             pairs-ordered-by-cube-sum)))
    (define (search weights)
      (let ((first (stream-car weights))
            (second (stream-car (stream-cdr weights))))
        (if (= first second)
            (cons-stream second
                         (search (stream-cdr (stream-cdr weights))))
            (search (stream-cdr weights)))))
    (search weights)))

(display-first-terms Ramanujan-numbers 5) ; 1729 4104 13832 20683 32832

; ex 3.72, numbers that can be written as sum of two squares in 3 different ways
(define (square-sum-weight x)
  (+ (square (car x))
     (square (cadr x))))

(define pairs-ordered-by-3.72
  (weighted-pairs integers
                  integers
                  square-sum-weight))

(define (search-3.72 pairs)
  (let ((first (stream-car pairs))
        (second (stream-car (stream-cdr pairs)))
        (third (stream-car (stream-cdr (stream-cdr pairs)))))
    (if (= (square-sum-weight first) (square-sum-weight second) (square-sum-weight third))
        (cons-stream (list (square-sum-weight first) first second third)
                     (search-3.72 (stream-cdr (stream-cdr (stream-cdr pairs)))))
        (search-3.72 (stream-cdr pairs)))))

(define numbers-3.72 (search-3.72 pairs-ordered-by-3.72))

(display-first-terms numbers-3.72 2) ; (325 (1 18) (6 17) (10 15)) (425 (5 20) (8 19) (13 16))

; streams and delayed evaluation
(define (integral delayed-integrand init-val dt)
  (define int
    (cons-stream init-val
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  ;;; error: cannot use before initialization
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.01) 100)

; ex 3.77
(define (integral-2 delayed-integrand init-val dt)
  (cons-stream init-val
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                     the-empty-stream
                     (integral (stream-cdr integrand)
                               (+ (* dt (stream-car integrand))
                                  init-val)
                               dt)))))

; ex 3.78
(define (solve-2nd a b y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a) (scale-stream y b)))
  y)

; ex 3.79
(define(solve-2nd-general f y0 dy0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

; modularity of functional programs and objects
(define rand-init 0)
(define (rand-update x) (inc x))

; using assignment
(define rand
  (let ((x rand-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

; using streams
(define rand-numbers
  (cons-stream rand-init
               (stream-map rand-update rand-numbers)))

(define (map-successive-pairs f s)
  (cons-stream
   (f (stream-car s) (stream-car (stream-cdr s)))
   (map-successive-pairs f (stream-cdr (stream-cdr s)))))

(define cesaro-stream
  (map-successive-pairs (lambda (r1 r2) (= (gcd r1 r2) 1))
                        rand-numbers))

(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ passed (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (inc passed) failed)
      (next passed (inc failed))))

(define pi
  (stream-map (lambda (p) (sqrt (/ 6 p) 1.0))
              (monte-carlo cesaro-stream 0 0)))

; ex 3.81
(define (rand-stream m)
  (define (rand-nums init)
    (define s
      (cons-stream init (stream-map rand-update s)))
    s)
  
  (cond ((eq? 'generate m) (rand-nums 0))
        ((eq? 'reset m) (lambda (new-value) (rand-nums new-value)))
        (else (error "message must be either generate or reset -- RAND-STREAM" m))))

(display-first-terms (rand-stream 'generate) 5)
(display-first-terms ((rand-stream 'reset) 99) 5)

; ex 3.82
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2)
  (define random-pairs
    (cons-stream (cons (random-in-range x1 y1)
                       (random-in-range x2 y2))
                 random-pairs))
  (let ((area (* (- x2 x1) (- y2 y1))))
    (scale-stream (monte-carlo (stream-map P random-pairs) 0 0) area)))

; a functional-programming view of time

; 1. model state with local variables, model the changes of state
; with assignment to thoses variables
(define (make-simplified-withdraw-OOP balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

; 2. use stream to represent the time history of successive states
(define (stream-withdraw-FP balance amount-stream)
  (cons-stream
   balance
   (stream-withdraw-FP (- balance (stream-car amount-stream))
                       (stream-cdr amount-stream))))