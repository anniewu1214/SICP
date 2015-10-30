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

; ex 3.54
(define (mul-streams s1 s2) (stream-map-general * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams (integers-starting-from 2) factorials)))

(stream-ref factorials 4)

; ex 3.55
(define (partial-sums S)
  (cons-stream (stream-car S)
               (add-streams (stream-cdr S)
                            (partial-sums S))))

(stream-ref (partial-sums integers) 4) ; 15

; ex 3.56
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
(define (print-stream-n s n)
  (if (> n 0)
      (begin (display (stream-car s))
             (display " ")
             (print-stream-n (stream-cdr s) (dec n)))))

(print-stream-n SS 10) (newline)