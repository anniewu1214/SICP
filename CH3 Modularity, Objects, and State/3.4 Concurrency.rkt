#lang planet neil/sicp

; ex 3.38
;
; a. Processes run sequentially: 45, 40, 35, 50,
; b. Processes interleaved: 110, 80, 90, 60, 30, 55
(define (parallel-execute . procedures) "execute procedures parallelly")

; ex 3.39
;
; 101: P1 -> P2
; 121: P2 -> P1
; 100: P1 accesses x -> P2 sets x to 11 -> P1 sets x to 100
(define x 10)
(define s (make-serializer))
(parallel-execute (lambda () (set! x ((s (lambda () (* x x))))))
                  (s (lambda () (set! x (+ x 1)))))

; ex 3.40
;
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
;
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
;
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

; complexity of using multiple shared resources
(define (make-account-and-serialier balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

; use both accounts' serializers to serialize exchange
(define (deposite account amount)
  (let ((s (account 'serializer))
        (d (account 'deposit)))
    ((s d) amount)))

(define (serialized-exchange account1 account2)
  (let ((s1 (account1 'serializer))
        (s2 (account2 'serializer)))
    ((s1 (s2 exchange)) account1 account2)))

; ex 3.43
;
; 1. if we don't serialize exchange:
; X exchanges a1, a2 ->
; diff = 10 - 20 = -10 ->
; Y exchanges a2, a3 ->
; diff = 20 - 30 = -10 ->
; X continue, a1 = 20, a2 = 10 ->
; Y continue, a2 = 20, a3 = 20 ->
; Final result: 20, 20, 20
;
; 2. The total amount does not change because every exchange adds
; an amount in one account and substracts the same amount in another.


; ex 3.44
;
; Transfer is correct, the difference is that transfer does not have intermediate
; actions, which can be interleaved in concurrency, that access balance.

; ex 3.45
;
; In exchange, the withdraw and deposit procedure returned by (account 'withdraw)
; and (account 'deposit) have already been locked (by the same serializer as exchange),
; thus the whole exchange procedure is locked and won't execute.

(define (make-serializer)
  ;;; once a mutex is acquired, no other acquire operations on that mutex
  ;;; may proceed until the mutex is released
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list #f))) ; false <=> not acquired, true <=> acquired
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire)))
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell #f))

; ex 3.46
;
; Suppose that P1 and P2 are both serialized by S, test-and-set! fails when:
; P1 acquires the mutex -> test-and-set! -> (car cell) = #t -> before P1
; finishes executing the begin block, P2 acquires the mutex -> (car cell) = #t
; => the mutex is acquired by both P1 and P2
;
; We must guarentee that once a process has tested the cell and found it to be false,
; the cell contents will actually be set to true before any other process can test the cell.
(define (test-and-set! cell)
  (if (car cell)
      #t
      (begin (set-car! cell true)
             #f)))

; *ex 3.47, a semaphore can be acquired concurrently by up to n processes
; implementation of semaphore using mutex
(define (make-semaphore n)
  (let ((mutex (make-mutex)) (count 0))
    (define (the-semaphore m)
      (mutex 'acquire) ; lock
      (cond ((eq? m 'acquire)
             (if (< count n)
                 (begin (set! count (inc count))
                        (mutex 'release)) ; release
                 (begin (mutex 'release)
                        (the-semaphore 'acquire))))
            ((eq? m 'release)
             (set! count (dec count))
             (mutex 'release))))
    the-semaphore))

; implementation of semaphore using atomic test-and-set!
(define (make-semaphore-2 n)
  (let ((count-locked (list #f)) (count 0))
    
    (define (lock-count)
      (if (test-and-set! count-locked)
          (lock-count)))
    
    (define (the-semaphore m)
      (lock-count) ; lock
      (cond ((eq? m 'acquire)
             (cond ((< count n)
                    (set! count (inc count))
                    (clear! count-locked)) ; release
                   (else
                    (clear! count-locked)
                    (the-semaphore 'acquire))))
            ((eq? m 'release)
             (set! count (dec count))
             (clear! count-locked))))
    the-semaphore))

; ex 3.48, deadlock-avoidance method
; If each process acquires the smallest-numbered account P, then P is 
; always locked firstly, other procedures can't proceed before P is released.
(define (make-account-no-deadlock id balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            ((eq? m 'id) id)
            (else (error "Unknown request -- MAKE-ACCOUNT" m))))
    dispatch))

(define (serialized-exchange-no-deadlock account1 account2)
  (let ((s1 (account1 'serializer))
        (s2 (account2 'serializer)))
    (if (< (account1 'id) (account2 'id))
        ((s2 (s1 exchange)) account1 account2)
        ((s1 (s2 exchange)) account1 account2))))