#lang planet neil/sicp

; GCD machine
(controller
 test-b
    (test (op =) (reg b) (const 0))
    (branch (label gcd-done))
    (assign t (op rem) (reg a) (reg b))
    (assign a (reg b))
    (assign b (reg a))
    (goto (label test-b))
 gcd-done)

; ex 5.1, ex 5.2
; factorial machine
(controller
    (assign product (const 1))
    (assign counter (const 1))
 test-counter
    (test (op >) (reg counter) (reg n))
    (branch (label fac-done))
    (assign product (op *) (reg product) (reg counter))
    (assign counter (op inc) (reg counter))
    (goto (label test-counter))
 fac-done)

; GCD machine that read inputs and prints results
(controller
 gcd-loop
    (assign a (op read))
    (assign b (op read))
 test-b   
    (test (op =) (reg b) (const 0))
    (branch (label gcd-done))
    (assign t (op rem) (reg a) (reg b))
    (assign a (reg b))
    (assign b (reg a))
    (goto (label test-b))
 gcd-done
    (perform (op print) (reg a))
    (goto (label gcd-loop)))

; elaborated GCD machine
(controller
 test-b
    (test (op =) (reg b) (const 0))
    (branch (label gcd-done))
    (assign t (reg a))
 rem-loop
    (test (op <) (reg t) (reg b))
    (branch (label rem-done))
    (assign t (op -) (reg t) (reg b))
    (goto (label rem-loop))
 rem-done
    (assign a (reg b))
    (assign b (reg t))
    (goto (label test-b))
 gcd-done)

; ex 5.3, square root register machine
(controller
    (assign x (op read))
    (assign guess (const 1.0))
 test-good
    (test (op good-enough?) (reg guess) (reg x))
    (branch (label sqrt-done))
    (assign guess (op improve) (reg guess) (reg x))
    (goto (label test-good))
 sqrt-done)

(controller
    (assign x (op read))
    (assign guess (const 1.0))
 test-good
    (assign temp (op *) (reg guess) (reg guess))
    (assign temp (op -) (reg temp) (reg x))
    (test (op >) (reg temp) (const 0))
    (branch (label abs-done))
    (assign temp (op -) (const 0) (reg temp))
 abs-done
    (test (op <) (reg temp) (const 0.001))
    (branch (label sqrt-done))
    (assign temp (op /) (reg x) (reg guess))
    (assign temp (op +) (reg temp) (reg guess))
    (assign temp (op /) (reg temp) (const 2))
    (goto (label test-good))
 sqrt-done)

; subroutines
(controller
 gcd
    (test (op =) (reg b) (const 0))
    (branch (label gcd-done))
    (assign t (op rem) (reg a) (reg b))
    (assign a (reg b))
    (assign b (reg a))
    (goto (label test-b))
 gcd-done
    (goto (reg continue))

    (assign continue (label after-gcd-1))
    (goto (label gcd))
 after-gcd-1
 
    (assign continue (label after-gcd-1))
    (goto (label gcd))
 after-gcd-2
)

; recursive factorial machine
(controller
    (assign continue (label fact-done))   ; set up final return address
 fact-loop
    (test (op =) (reg n) (const 1))
    (branch (label base-case))
    ;; set up for the recursive call by saving n and continue.
    ;; set up continue so that the computation will continue
    ;; at after-fact when the subroutie returns
    (save continue)
    (save n)
    (assign n (op -) (reg n) (const 1))
    (assign continue (label after-fact))
    (goto (label fact-loop))
  after-fact
    (restore n)
    (restore continue)
    (assign val (op *) (reg n) (reg val)) ; val now contains n(n-1)!
    (goto (reg continue))                 ; return to caller
  base-case
    (assign val (const 1))                ; base case: 1! = 1
    (goto (reg continue))                 ; return to caller
  fact-done)

; ex 5.4
; recursive exponentiation machine
(controller
   (assign continue (label exp-done))
 exp-loop
   (test (op =) (reg n) (const 0))
   (branch (label base-case))
   (save continue)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label after-exp))
   (goto (label exp-loop))
 after-exp
   (restore continue)
   (assign val (op *) (reg b) (reg val))
   (goto (reg continue))
 base-case
   (assign val (const 1))
   (goto (reg continue))
 exp-done)

; iterative exponentiation machine
(controller
  (assign product (const 1))
 exp-loop
  (test (op =) (reg n) (const 0))
  (branch (label exp-done))
  (assign product (op *) (reg b) (reg product))
  (assign n (op -) (reg n) (const 1))
  (goto (label exp-loop))
 exp-done)

; ex 5.6, Fibonacci machine
(controller
   (assign continue (label fib-done))
 fib-loop
   (test (op <) (reg n) (const 2))
   (branch (label immediate-answer))
   ;; set up to compute Fib(n - 1)
   (save continue)
   (assign continue (label afterfib-n-1))
   (save n)                           ; save old value of n
   (assign n (op -) (reg n) (const 1)); clobber n to n - 1
   (goto (label fib-loop))            ; perform recursive call
 afterfib-n-1                         ; upon return, val contains Fib(n - 1)
   (restore n)
   ;(restore continue)
   ;; set up to compute Fib(n - 2)
   (assign n (op -) (reg n) (const 2))
   ;(save continue)
   (assign continue (label afterfib-n-2))
   (save val)                         ; save Fib(n - 1)
   (goto (label fib-loop))
 afterfib-n-2                         ; upon return, val contains Fib(n - 2)
   (assign n (reg val))               ; n now contains Fib(n - 2)
   (restore val)                      ; val now contains Fib(n - 1)
   (restore continue)
   (assign val                        ; Fib(n - 1) +  Fib(n - 2)
           (op +) (reg val) (reg n)) 
   (goto (reg continue))              ; return to caller, answer is in val
 immediate-answer
   (assign val (reg n))               ; base case:  Fib(n) = n
   (goto (reg continue))
 fib-done)