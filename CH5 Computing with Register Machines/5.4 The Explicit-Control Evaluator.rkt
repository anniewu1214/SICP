#lang planet neil/sicp

;; the core of the explicit-control evaluator

(define eceval
  (make-machine
   eceval-operations
   '(
     ;; running the evaluator
     read-eval-print-loop
     (perform (op initialize-stack))
     (perform (op prompt-for-input) (const ";;; EC-Eval input:"))
     (assign exp (op read))
     (assign env (op get-global-environment))
     (assign continue (label print-result))
     (goto (label eval-dispatch))
     
     print-result
     (perform (op print-stack-statistics)) ; added instruction
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
     (test (op cond?) (reg exp)) ; ex 5.23
     (branch (label ev-cond))
     (test (op let?) (reg exp))  ; ex 5.23
     (branch (label ev-let))
     (goto (label unknown-expression-type))
     
     
     ;; evaluating simple expressions (with no subexpressions)
     ev-self-eval
     (assign val (reg exp))
     (goto (reg continue))

     ;; ex 5.30
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
     ;; ex 5.32
     ;; avoid saving and restoring env around the evaluation
     ;; of the operator if the operator is a symbol
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
     (goto (label unknown-procedure-type))

     ;; ex 5.30
     ;; handling primitive procedure errors
     primitive-apply
     (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
     (test (op error?) (reg val))
     (branch (label primitive-apply-error))
     (restore continue)
     (goto (reg continue))

     primitive-apply-error
     (restore continue)  ; clean up stack
     (assign val (op error-type) (reg val))
     (goto (label signal-error))
     
     compound-apply
     (assign unev (op procedure-parameters) (reg proc))
     (assign env (op procedure-environment) (reg proc))
     (assign env (op extend-environment) (reg unev) (reg argl) (reg env))
     (assign unev (op procedure-body) (reg proc))
     (goto (label ev-sequence))
     
     
     ;; evaluating sequence with tail recursion
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
     
     
;     ;; evaluating sequence without tail recursion
;     ev-sequence
;     (test (op no-more-exps?) (reg unev))
;     (branch (label ev-sequence-end))
;     (assign exp (op first-exp) (reg unev))
;     (save unev)
;     (save env)
;     (assign continue (label ev-sequence-continue))
;     (goto (label eval-dispatch))
;     
;     ev-sequence-continue
;     (restore env)
;     (restore unev)
;     (assign unev (op rest-exps) (reg unev))
;     (goto (label ev-sequence))
;     
;     ev-sequence-end
;     (restore continue)
;     (goto (reg continue))
     
     
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
     
     
     ;; ex 5.24, condition as special form
     ev-cond-ex24
     (assign unev (op cond-clauses) (reg exp))
     (save continue)
     
     ev-cond-loop
     (test (op null?) (reg unev))
     (branch (label ev-cond-void))
     (assign exp (op cond-first-clause-predicate) (reg unev))
     (test (op cond-else-clause?) (reg exp))
     (branch (label ev-cond-true))
     (save unev)
     (save env)
     (assign continue (label ev-cond-1))
     (goto (label eval-dispatch))
     
     ev-cond-1
     (restore env)
     (restore unev)
     (test (op true?) (reg val))
     (branch (label ev-cond-true))
     (assign unev (op cond-rest-clauses) (reg unev))
     (goto (label ev-cond-loop))
     
     ev-cond-true
     (assign unev (op cond-first-clause-actions) (reg unev))
     (goto (label ev-sequence))
     
     ev-cond-void
     (assign val (const false))
     (restore continue)
     (goto (reg continue))
     
     ;; let
     ev-let
     (assign exp (op let->combination) (reg exp))
     (goto (label ev-lambda))
     
     )))



;; start the evaluator
(start eceval)


;; append
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(append '(a b c) '(d e f))



;; ex 5.26, ex 5.27
;
; (factorial-iter 1) -> (total-pushes = 64  maximum-depth = 10)
; (factorial-iter 2) -> (total-pushes = 99  maximum-depth = 10)
; (factorial-iter 3) -> (total-pushes = 134 maximum-depth = 10)
; (factorial-iter 4) -> (total-pushes = 169 maximum-depth = 10)
;
; (factorial-rec 1)  -> (total-pushes = 16  maximum-depth = 8)
; (factorial-rec 2)  -> (total-pushes = 48  maximum-depth = 13)
; (factorial-rec 3)  -> (total-pushes = 80  maximum-depth = 18)
; (factorial-rec 4)  -> (total-pushes = 112 maximum-depth = 23)
;
;
;                      | maximum-depth      | total-pushes
;                      | (space complexity) | (time complexity) 
; ---------------------|--------------------|-------------------
; recursive factorial  | 5n + 3             |  32n - 16
; iterative factorial  | 10                 |  35n + 29


(define (factorial-iter n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

(define (factorial-rec n)
  (if (= n 1)
      1
      (* (factorial-rec (- n 1)) n)))



;; ex 5.28
;; eval-sequence no longer tail-recursive
;
; (factorial-iter 1) -> (total-pushes = 70  maximum-depth = 17)
; (factorial-iter 2) -> (total-pushes = 107 maximum-depth = 20)
; (factorial-iter 3) -> (total-pushes = 144 maximum-depth = 23)
; (factorial-iter 4) -> (total-pushes = 181 maximum-depth = 26)
;
; (factorial-rec 1)  -> (total-pushes = 18  maximum-depth = 11)
; (factorial-rec 2)  -> (total-pushes = 52  maximum-depth = 19)
; (factorial-rec 3)  -> (total-pushes = 86  maximum-depth = 27)
; (factorial-rec 4)  -> (total-pushes = 120 maximum-depth = 35)
;
;
;                      | maximum-depth      | total-pushes
; ---------------------|--------------------|-------------------
; recursive factorial  | 37n + 33           |  3n + 14
; iterative factorial  | 34n - 16           |  8n + 3



; ex 5.29
;
; (fib 0) -> (total-pushes = 16 maximum-depth = 8)
; (fib 1) -> (total-pushes = 16 maximum-depth = 8)
; (fib 2) -> (total-pushes = 72 maximum-depth = 13)
; (fib 3) -> (total-pushes = 128 maximum-depth = 18)
; (fib 4) -> (total-pushes = 240 maximum-depth = 23)
; (fib 5) -> (total-pushes = 408 maximum-depth = 28)
;
;
;                      | maximum-depth      | total-pushes
;                      | (linear)           | (exponential)
; ---------------------|--------------------|--------------------
; recursive Fibonacci  | 5n + 3             |  56 * Fib(n+1) - 40

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))



;; ex 5.30
;
; a. procedure lookup-variable-value and ev-variable modified.
; b. new safe-car and safe-div procedure, primitive-apply modified
;
;; test in REPL
a          ; unbounded-variable-error
(car 'a)   ; car-on-non-pair-error
(/ 1 0)    ; zero-division-error