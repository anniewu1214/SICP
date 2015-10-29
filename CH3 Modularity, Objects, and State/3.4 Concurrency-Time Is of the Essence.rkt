#lang planet neil/sicp

; ex 3.38
; a. Processes run sequentially: 45, 40, 35, 50,
; b. Processes interleaved: 110, 80, 90, 60, 30, 55

; ex 3.39
; 101: P1 -> P2
; 121: P2 -> P1
; 100: P1 accesses x -> P2 sets x to 11 -> P1 sets x to 100
(define x 10)
(define s (make-serializer))
(parallel-execute (lambda () (set! x ((s lambda () (* x x)))))
                  (s (lambda () (set! x (+ x 1)))))


; ex 3.40
; 10^6: P1 -> P2, or P2 -> P1
; 10^2: P1 accesses x -> P2 sets x to 10^3 -> P1 sets x to 10^2
; 10^3: P2 accesses x -> P1 sets x to 10^2 -> P2 sets x to 10^3
; 10*10^3, 10*10*10^2, 10*10^2*10^2: when P1 interleave with P2
; after serializing: 10^6
(parallel-execute (lambda () (set! x (* x x)))
                  (lambda () (set! x (* x x x))))

(parallel-execute (s (lambda () (set! x (* x x))))
                  (s (lambda () (set! x (* x x x)))))


; ex 3.41
; No, only procedures with mutations need to be protected
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance) ; serialization unnecessary
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

; ex 3.42
; There's no difference in concurrency 
(define (make-account-3.42 balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (let ((protected-withdraw (protected withdraw))
          (protected-deposit (protected deposit)))
      (define (dispatch m)
        (cond ((eq? m 'withdraw) protected-withdraw)
              ((eq? m 'deposit) protected-deposit)
              ((eq? m 'balance) balance)
              (else (error "Unknown request -- MAKE-ACCOUNT" m))))
      dispatch)))