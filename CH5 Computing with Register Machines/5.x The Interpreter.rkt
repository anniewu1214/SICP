#lang planet neil/sicp

;; the core of the interpreter

(define eceval
  (make-machine
   eceval-operations
   '(
     (branch (label external-entry))
     
     ;; running the evaluator
     read-eval-print-loop
     (perform (op initialize-stack))
     (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))
     
     print-result
     (perform (op print-stack-statistics))
     (perform (op announce-output) (const ";;; EC-Eval value:"))
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     
     
     ;; error
     unknown-expression-type
     (assign val (const unknown-expression-type-error))
     (goto (label signal-error))
     
     unknown-procedure-type
     (restore continue)
     (assign val (const unknown-procedure-type-error))
     (goto (label signal-error))
     
     signal-error
     (perform (op user-print) (reg val))
     (goto (label read-eval-print-loop))
     
     
     ;; syntactic analysis of expressions
     eval-dispatch
     (test (op self-evaluating?) (reg exp))
     (branch (label ev-self-eval))
     (test (op variable?) (reg exp))
     (branch (label ev-variable))
     (test (op quoted?) (reg exp))
     (branch (label ev-quoted))
     (test (op assignment?) (reg exp))
     (branch (label ev-assignment))
     (test (op definition?) (reg exp))
     (branch (label ev-definition))
     (test (op if?) (reg exp))
     (branch (label ev-if))
     (test (op lambda?) (reg exp))
     (branch (label ev-lambda))
     (test (op begin?) (reg exp))
     (branch (label ev-begin))
     (test (op application?) (reg exp))
     (branch (label ev-application))
     (test (op cond?) (reg exp))
     (branch (label ev-cond))
     (test (op let?) (reg exp))
     (branch (label ev-let))
     (goto (label unknown-expression-type))
     
     
     ;; evaluating simple expressions
     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))
     
     
     ;; catching unbounded variable error
     ev-variable
     (assign val (op lookup-variable-value) (reg exp) (reg env))
     (test (op error?) (reg val))
     (branch (label unbound-variable))
     (goto (reg continue))
     
     unbound-variable
     (assign val (op error-type) (reg val))
     (goto (label signal-error))
     
     ev-quoted
     (assign val (op text-of-quotation) (reg exp))
     (goto (reg continue))
     
     ev-lambda
     (assign unev (op lambda-parameters) (reg exp))
     (assign exp (op lambda-body) (reg exp))
     (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
     (goto (reg continue))
     
     
     ;; evaluating procedure application
     ev-application
     (save continue)
     (assign unev (op operands) (reg exp))
     (assign exp (op operator) (reg exp))
     
     ;; don't save/restore env if operator is symbol
     (test (op symbol?) (reg exp))
     (branch (label ev-operator-symbol))
     (save env)
     (save unev)
     (assign continue (label ev-appl-did-operator))
     (goto (label eval-dispatch))
     
     ev-operator-symbol
     (assign continue (label ev-operator-symbol-1))
     (goto (label eval-dispatch))
     
     ev-appl-did-operator
     (restore unev)
     (restore env)
     
     ev-operator-symbol-1
     (assign argl (op empty-arglist))
     (assign proc (reg val))
     (test (op no-operands?) (reg unev))
     (branch (label apply-dispatch))
     (save proc)
     
     ev-appl-operand-loop
     (save argl)
     (assign exp (op first-operand) (reg unev))
     (test (op last-operand?) (reg unev))
     (branch (label ev-appl-last-arg))
     (save env)
     (save unev)
     (assign continue (label ev-appl-accumulate-arg))
     (goto (label eval-dispatch))
     
     ev-appl-accumulate-arg
     (restore unev)
     (restore env)
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (assign unev (op rest-operands) (reg unev))
     (goto (label ev-appl-operand-loop))
     
     ev-appl-last-arg
     (assign continue (label ev-appl-accum-last-arg))
     (goto (label eval-dispatch))
     
     ev-appl-accum-last-arg
     (restore argl)
     (assign argl (op adjoin-arg) (reg val) (reg argl))
     (restore proc)
     (goto (label apply-dispatch))
     
     
     ;; applying procedure to arguments
     apply-dispatch
     (test (op primitive-procedure?) (reg proc))
     (branch (label primitive-apply))
     (test (op compound-procedure?) (reg proc))  
     (branch (label compound-apply))
     (test (op compiled-procedure?) (reg proc))  
     (branch (label compiled-apply))
     (goto (label unknown-procedure-type))
     
     
     compiled-apply
     (restore continue)
     (assign val (op compiled-procedure-entry) (reg proc))
     (goto (reg val))
     
     
     ;; handling primitive procedure errors
     primitive-apply
     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
     (test (op error?) (reg val))
     (branch (label primitive-apply-error))
     (restore continue)
     (goto (reg continue))
     
     primitive-apply-error
     (restore continue)
     (assign val (op error-type) (reg val))
     (goto (label signal-error))
     
     compound-apply
     (assign unev (op procedure-parameters) (reg proc))
     (assign env (op procedure-environment) (reg proc))
     (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
     (assign unev (op procedure-body) (reg proc))
     (goto (label ev-sequence))
     
     
     ;; evaluating sequence
     ev-begin
     (assign unev (op begin-actions) (reg exp))
     (save continue)
     (goto (label ev-sequence))
     
     ev-sequence
     (assign exp (op first-exp) (reg unev))
     (test (op last-exp?) (reg unev))
     (branch (label ev-sequence-last-exp))
     (save unev)
     (save env)
     (assign continue (label ev-sequence-continue))
     (goto (label eval-dispatch))
     
     ev-sequence-continue
     (restore env)
     (restore unev)
     (assign unev (op rest-exps) (reg unev))
     (goto (label ev-sequence))
     
     ev-sequence-last-exp
     (restore continue)
     (goto (label eval-dispatch))
     
     
     ;; if
     ev-if
     (save exp)
     (save env)
     (save continue)
     (assign continue (label ev-if-decide))
     (assign exp (op if-predicate) (reg exp))
     (goto (label eval-dispatch))
     
     ev-if-decide
     (restore continue)
     (restore env)
     (restore exp)
     (test (op true?) (reg val))
     (branch (label ev-if-consequent))
     
     ev-if-alternative
     (assign exp (op if-alternative) (reg exp))
     (goto (label eval-dispatch))
     
     ev-if-consequent
     (assign exp (op if-consequent) (reg exp))
     (goto (label eval-dispatch))
     
     
     ;; assingment
     ev-assignment
     (assign unev (op assignment-variable) (reg exp))
     (save unev)
     (assign exp (op assignment-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-assignment-1))
     (goto (label eval-dispatch))
     
     ev-assignment-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op set-variable-value!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))
     
     
     ;; definition
     ev-definition
     (assign unev (op definition-variable) (reg exp))
     (save unev)
     (assign exp (op definition-value) (reg exp))
     (save env)
     (save continue)
     (assign continue (label ev-definition-1))
     (goto (label eval-dispatch))
     
     ev-definition-1
     (restore continue)
     (restore env)
     (restore unev)
     (perform
      (op define-variable!) (reg unev) (reg val) (reg env))
     (assign val (const ok))
     (goto (reg continue))
     
     
     ;; condition
     ev-cond
     (assign exp (op cond->if) (reg exp))
     (goto (label ev-if))
     
     
     ;; let
     ev-let
     (assign exp (op let->combination) (reg exp))
     (goto (label ev-lambda))
     
     external-entry
     (perform (op initialize-stack))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (reg val)))))


;; start the evaluator at its ordinary REPL
(define (start-eceval)
  (set! the-global-environment (setup-environment))
  (set-register-contents! eceval 'flag false)
  (start eceval))



;; start the evaluator
(define (compile-and-go expression)
  (let ((instructions
         (assemble (statements (compile expression 'val 'return '()))
                   eceval)))
    (set! the-global-environment (setup-environment))
    (set-register-contents! eceval 'val instructions)
    (set-register-contents! eceval 'flag true)
    (start eceval)))




;; test

(compile-and-go
 '(define (factorial n)
    (if (= n 1)
        1
        (* (factorial (- n 1)) n))))



;; ex 5.45
;
; compiled factorial:
; (factorial 2) -> (total-pushes = 5  maximum-depth = 3)
; (factorial 3) -> (total-pushes = 7  maximum-depth = 4)
; (factorial 4) -> (total-pushes = 9  maximum-depth = 6)
; (factorial 5) -> (total-pushes = 11 maximum-depth = 8)
; (factorial 6) -> (total-pushes = 13 maximum-depth = 10)
;
; total-pushes = 2n + 1, maximum-depth = 2n - 2
;
; interpreted factorial:
; total-pushes = 32n - 16, maximum-depth = 5n + 3
;
; hand-tailored factorial:
; total-pushes = 2n - 2, maximum-depth = 2n - 2
;
; conclusion:
; compilation is 16 times faster than interpretation, and as fast as the hand-tailored factorial
; compilation takes 2.5 times less space than interpretation, ans as much space as the hand-tailored factorial



;; ex 5.46
(compile-and-go
 '(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2))))))

; (fib 1) -> (total-pushes = 5   maximum-depth = 3)
; (fib 2) -> (total-pushes = 13  maximum-depth = 4)
; (fib 3) -> (total-pushes = 21  maximum-depth = 6)
; (fib 4) -> (total-pushes = 37  maximum-depth = 8)
; (fib 5) -> (total-pushes = 61  maximum-depth = 10)
;
; total-pushes = 8 * Fib(n) - 3, maximum-depth = 2n
;
; interpreted fib:
; total-pushes = 56 * Fib(n) - 40, maximum-depth = 5n + 3
;
; hand-tailored factorial:
; total-pushes = 3 * Fib(n) - 3, maximum-depth = 2n - 2
;
; conclusion
; compilation is 7 times faster than interpretation, and 2.67 times slower the hand-tailored factorial
; compilation takes 2.5 times less space than interpretation, ans as much space as the hand-tailored factorial