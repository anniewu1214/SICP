#lang planet neil/sicp

;; GCD machine
(define gcd-machine
  (make-machine
   (list (list 'rem remainder) (list '= =))
   '(test-b
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done)))

(set-register-contents! gcd-machine 'a 206)
(set-register-contents! gcd-machine 'b 40)
(start gcd-machine)
(get-register-contents gcd-machine 'a) ; 2

;; ex 5.1, ex 5.2
;; factorial machine
(define fac-machine
  (make-machine
   (list (list '* *) (list '+ +) (list '> >))
   '((assign product (const 1))
     (assign counter (const 1))
     test-counter
     (test (op >) (reg counter) (reg n))
     (branch (label fac-done))
     (assign product (op *) (reg product) (reg counter))
     (assign counter (op +) (reg counter) (const 1))
     (goto (label test-counter))
     fac-done)))

(set-register-contents! fac-machine 'n 5)
(start fac-machine)
(get-register-contents fac-machine 'product) ; 120

;; GCD machine that read inputs and prints results
(define gcd-machine-read-inputs
  (make-machine
   (list (list 'read read) (list '= =) (list 'rem remainder) (list 'print display))
   '(gcd-loop
     (assign a (op read))
     (assign b (op read))
     test-b   
     (test (op =) (reg b) (const 0))
     (branch (label gcd-done))
     (assign t (op rem) (reg a) (reg b))
     (assign a (reg b))
     (assign b (reg t))
     (goto (label test-b))
     gcd-done
     (perform (op print) (reg a))
     (goto (label gcd-loop)))))

(start gcd-machine-read-inputs)


;; elaborated GCD machine
(define elaborated-gcd-machine
  (make-machine
   (list (list 'rem remainder) (list '= =) (list '< <) (list '- -))
   '(test-b
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
     gcd-done)))

(set-register-contents! elaborated-gcd-machine 'a 206)
(set-register-contents! elaborated-gcd-machine 'b 40)
(start elaborated-gcd-machine)
(get-register-contents elaborated-gcd-machine 'a) ; 2

;; ex 5.3
;; square root register machine
(define (square x) (* x x))
(define (average a b) (/ (+ a b) 2))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))

(define sqrt-machine
  (make-machine
   (list (list 'good-enough? good-enough?) (list 'read read) (list 'improve improve))
   '((assign x (op read))
     (assign guess (const 1.0))
     test-good
     (test (op good-enough?) (reg guess) (reg x))
     (branch (label sqrt-done))
     (assign guess (op improve) (reg guess) (reg x))
     (goto (label test-good))
     sqrt-done)))

(start sqrt-machine)
(get-register-contents sqrt-machine 'guess)

;; elaborated square root register machine
(define elaborated-sqrt-machine
  (make-machine
   (list (list 'read read) (list '* *) (list '> >) (list '< <)
         (list '/ /) (list '+ +) (list '- -) (list 'print display))
   '(sqrt-loop
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
     (assign guess (op /) (reg temp) (const 2))
     (goto (label test-good))
     sqrt-done
     (perform (op print) (reg guess))
     (goto (label sqrt-loop)))))

(start elaborated-sqrt-machine)

;; subroutines
;; recursive factorial machine
(define recursive-fact-machine
  (make-machine
   (list (list '= =) (list '- -) (list '* *))
   '((assign continue (label fact-done))   ; set up final return address
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
     fact-done)))

(set-register-contents! recursive-fact-machine 'n 5)
(start recursive-fact-machine)
(get-register-contents recursive-fact-machine 'val) ; 120

; ex 5.4, ex 5.7
; recursive exponentiation machine
(define recursive-exp-machine
  (make-machine
   (list (list '= =) (list '- -) (list '* *))
   '((assign continue (label exp-done))
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
     exp-done)))

(set-register-contents! recursive-exp-machine 'b 2)
(set-register-contents! recursive-exp-machine 'n 10)
(start recursive-exp-machine)
(get-register-contents recursive-exp-machine 'val) ; 1024

; iterative exponentiation machine
(define iterative-exp-machine
  (make-machine
   (list (list '= =) (list '- -) (list '* *))
 '((assign product (const 1))
 exp-loop
 (test (op =) (reg n) (const 0))
 (branch (label exp-done))
 (assign product (op *) (reg b) (reg product))
 (assign n (op -) (reg n) (const 1))
 (goto (label exp-loop))
 exp-done)))

(set-register-contents! iterative-exp-machine 'b 2)
(set-register-contents! iterative-exp-machine 'n 10)
(start iterative-exp-machine)

(get-register-contents iterative-exp-machine 'product) ; 1024

; ex 5.6, Fibonacci machine
(define fib-machine
  (make-machine
   (list (list '< <) (list '- -) (list '+ +))
'(
 (assign continue (label fib-done))
 fib-loop
 (test (op <) (reg n) (const 2))
 (branch (label immediate-answer))
 (save continue)
 (assign continue (label afterfib-n-1))
 (save n)
 (assign n (op -) (reg n) (const 1))
 (goto (label fib-loop))
 afterfib-n-1
 (restore n)
 ;(restore continue)
 (assign n (op -) (reg n) (const 2))
 ;(save continue)
 (assign continue (label afterfib-n-2))
 (save val)
 (goto (label fib-loop))
 afterfib-n-2
 ;(assign n (reg val))
 ;(restore val)
 ;; ex 5.11, replace the above two instructions with
 (restore n)
 (restore continue)
 (assign val (op +) (reg val) (reg n)) 
 (goto (reg continue))
 immediate-answer
 (assign val (reg n))
 (goto (reg continue))
 fib-done)))

(set-register-contents! fib-machine 'n 7)
(start fib-machine)
(get-register-contents fib-machine 'val) ; 13