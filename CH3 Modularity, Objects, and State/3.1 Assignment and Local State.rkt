#lang planet neil/sicp

; ex 3.1
(define (make-accumulator n)
  (lambda (m)
    (set! n (+ n m))
    n))

(define A (make-accumulator 5))
(A 10)
(A 10)

; ex 3.2
(define (make-monitored f)
  (let ((count 0))
    (define (mf m)
      (cond ((eq? m 'how-many-calls?) count)
            ((eq? m 'reset-count) (set! count 0))
            (else (begin (set! count (+ count 1)) (f m)))))
    mf))

(define s (make-monitored sqrt))
(s 100)
(s 25)
(s 'how-many-calls?)

; ex 3.3, ex 3.4
(define (make-account balance pwd)
  (let ((access-times 0) (limit 7))
    (define (withdraw amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
    
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    
    (define (call-the-cops) "Cops' comin'")
    
    (define (dispatch p m)
      (if (eq? p pwd)
          (begin (set! access-times 0)
                 (cond ((eq? m 'withdraw) withdraw)
                       ((eq? m 'deposit) deposit)
                       (else (error "Unknown request -- MAKE-ACCOUNT" m))))
          (lambda (amount)
            (if (= limit access-times)
                (call-the-cops)
                (begin (set! access-times (+ 1 access-times))
                       "Incorrect password")))))
    dispatch))

(define acc (make-account 100 'secret-password))
((acc 'secret-password 'withdraw) 40)
((acc 'false-password 'withdraw) 50)

; ex 3.5
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0) (/ trials-passed trials))
          ((experiment) (iter (dec trials-remaining) (inc trials-passed)))
          (else (iter (dec trials-remaining) trials-passed))))
  (iter trials 0))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (estimate-integral P x1 x2 y1 y2 trials)
  (define (integral-test) (P (random-in-range x1 x2) (random-in-range y1 y2)))
  (monte-carlo trials integral-test))

(define (square x) (* x x))

(define (estimate-pi trials)
  (* 4 (estimate-integral (lambda (x y) (<= (+ (square x) (square y)) 1))
                          -1 1 -1 1 trials)))

(estimate-pi 1000)

; ex 3.6
(define random-init 0)
(define (rand-update x) (inc x))

(define rand  
  (let ((x random-init))
    (define (dispatch m)
      (cond ((eq? 'generate m)
             (begin (set! x (rand-update x))
                    x))
            ((eq? 'reset m)
             (lambda (new-value) (set! x new-value)))))
    dispatch))

(rand 'generate)
(rand 'generate)
((rand 'reset) 99)
(rand 'generate)


; ex 3.7
(define (make-joint account old-pwd new-pwd)
  ((account old-pwd 'withdraw) 0) ; test old-pwd
  (lambda (p m)
    (if (eq? p new-pwd)
        (account old-pwd m)
        (error "Incorrect Password" p))))

; ex 3.8
(define f
  (let ((init 999))
    (lambda (x)
      (if (= init 999)
          (begin (set! init x) x)
          0))))

(+ (f 0) (f 1))