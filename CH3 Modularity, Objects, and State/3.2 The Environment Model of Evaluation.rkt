#lang planet neil/sicp

; ex 3.9
; fac-re is bound to the corresponding procedure object in the global env.
; Calling (fac-re 6) creates a new env E1 in which n is bound to 6.
(define (fac-re n)
  (if (= n 1)
      1
      (* n (fac-re (dec n)))))

; fac-it is a symbol in the global env that is bound to a procedure object.
; Calling (fac-it 6) creates a new env E1, subordinated to the global env, in
; which the parameter n is bound to 6. The body of fac-it is then evaluated,
; the procedure iter will then be defined in E1. Then (iter 1 1 n) will be
; evaluated in E1. This will create a new env E2 in which product and counter
; are bound to 1.
(define (fac-it n)
  (define (iter product counter max-count)
    (if (> counter max-count)
        product
        (iter (* counter product) (inc counter) max-count)))
  (iter 1 1 n))

; ex 3.10
(define (make-withdraw init-amount)
  (let ((balance init-amount))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

; make-withdraw is equivalent to the following procedure. Calling (define W1
; (make-withdraw 100)) will set up E1 in which init-amount is bound to 100.
; Evaluating the body creates E2 subordinated to E1 in which balance is bound
; to 100, as well as an object procedure whose parameter is amount. Using let
; will create a supplementary env containing init-amount.
(define (make-withdraw-equiv init-amount)
  ((lambda (balance)
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds")))
   init-amount))


; ex 3.11
; 1. (define acc (make-account 50)) => set up E1 in which balance is bound
; to 50, where the symbols withdraw, deposit, dispatch are bound to their
; corresponding procedure objects whose associated env is E1. Within E1, we
; evaluate the body of make-account, which constructs a new procedure object
; dispatch.
;
; 2. ((acc 'deposit) 40) => we firstly evaluate (acc 'deposit), which creates
; E2, whose encloing env is E1, in which m is bound to 'deposit. Within E2 we
; evaluate the body who calls (deposit 40). Then a new env E3 is created, whose
; closing env is also E1, in which amount is bound to 40. Evaluating the body 
; will execute set!, which will change the binding of balance in E1 to 90.
;
; 3. (define acc2 (make-account 100)) => This will create a new env E4 similar
; to E1 except that balance is bound to 100. acc and acc2 have the same parameter
; and body. Calls to acc, acc2 reference the state variable in E1 and E2 respectively
; Changes to the local state of one object do not affect the other object.

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT" m))))
  dispatch)