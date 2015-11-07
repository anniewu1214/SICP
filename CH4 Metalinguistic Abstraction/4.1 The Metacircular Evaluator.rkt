#lang planet neil/sicp

; procedure arguments
(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

; ex 4.1
; explicitely determine the order of evaluation of operands
(define (list-of-values-left-to-right exps env)
  (if (no-operands? exps)
      '()
      (let ((first (eval (first-operand exps) env)))
        (let ((rest (list-of-values-left-to-right (rest-operands exps) env)))
          (cons first rest)))))

(define (list-of-values-right-to-left exps env)
  (if (no-operands? exps)
      '()
      (let ((rest (list-of-values-left-to-right (rest-operands exps) env)))
        (let ((first (eval (first-operand exps) env)))
          (cons first rest)))))

; conditionals
(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

; sequences
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exp exps) env))))

; assignments and definitions
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env))

; representing expressions
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

; (quote <text-of-quotation>)
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

; (set! <var> <val>)
(define (assignment? exp) (tagged-list? exp 'set!)) 
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

; (define <var> <val>)
; (define (<var> <par1> ... <parn>) <body>)
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; parameters
                   (cddr exp)))) ; body

; (lambda (p1 ... p2) <body>)
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

; (if predicate consequent alternative)
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

; (begin e1 e2 ... en)
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exp seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

; procedure application, (operator operand-1 ... operand-n)
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))

; ex 4.2
;
; a. this modificiation will identify assignments and definitions as applications,
; e.g. (define x 3) would be recognized as a procedure named define which takes two
; arguments, and this will result an error.
;
; b. (call operator operand-1 ... operand-n)
(define (application?-4.2 exp) (tagged-list? 'call))
(define (operator-4.2 exp) (cadr exp))
(define (operands-4.2 exp) (cddr exp))

; derived expressions
; (cond ((p-1) actions-1) ... ((p-n) actions-n) (else actions-else))
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))

; ex 4.5
; additonal syntax (<test> => <recipient>) for cond clauses
(define (sequence->exp-2 predicate actions)
  (if (eq? (car actions) '=>)
      (list (cadr actions) predicate)
      (sequence->exp actions)))

(define (expand-clauses clauses)
  (if (null? clauses) ; no else clause
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF" clauses))
            (make-if (cond-predicate first)
                     (sequence->exp-2 (cond-predicate first)
                                      (cond-actions first))
                     (expand-clauses rest))))))

; ex 4.4
; a. (and e1 e2 ... en)
(define (and? exp) (tagged-list? exp 'and))
(define (or? exp) (tagged-list? exp 'or))

(define (eval-and exp env)
  (define (iter seq)
    (cond ((null? seq) true)
          ((true? (eval (first-exp seq) env))
           (iter (rest-exp seq)))
          (else false)))
  (iter (cdr exp)))

(define (eval-or exp env)
  (define (iter seq)
    (cond ((null? seq) false)
          ((true? (eval (first-exp seq) env)) true)
          (else (iter (rest-exp seq)))))
  (iter (cdr exp)))

; b. implement and/or as derived expressions
(define (and->if exp) (expand-and-clauses (cdr exp)))
(define (or->if exp) (expand-or-clauses (cdr exp)))

(define (expand-and-clauses clauses)
  (if (null? clauses)
      'true
      (make-if (car clauses)
               (expand-and-clauses (cdr clauses))
               'false)))

(define (expand-or-clauses clauses)
  (if (null? clauses)
      'false
      (make-if (car clauses)
               'true
               (expand-or-clauses (cdr clauses)))))

; ex 4.6
; (let ((var-1 exp-1) ... (var-n exp-n)) <body>)
(define (let? exp) (tagged-list? exp 'let))
(define (let-vars exp) (map car (cadr exp)))
(define (let-exps exp) (map cadr (cadr exp)))
(define (let-body exp) (cddr exp))
(define (let->combination exp)
  (cons (make-lambda
         (let-vars exp)
         (let-body exp))
        (let-exps exp)))

; ex 4.7
; (let* ((v1 e1) (v2 e2) (vn en)) <body>)
; => (let ((v1 e1))
;      (let ((v2 e2))
;        (let ((v3 e3))
;          <body>)))
(define (let*? exp) (tagged-list? exp 'let*))
(define let*-body let-body)
(define (let*->nested-lets exp)
  (let ((body (let*-body exp)))
    (define (iter bindings)
      (if (null? (cdr bindings))
          (list 'let bindings body)
          (list 'let (list (car bindings)) (iter (cdr bindings)))))
    (iter (cadr exp))))

; ex 4.8
; named let: (let <var> <bindings> <body>)
(define (named-let? exp) (and (let? exp) (symbol? (cadr exp))))
(define (named-let-name exp) (cadr exp))
(define (named-let-body exp) (cadddr exp))
(define (named-let-vars exp) (map car (caddr exp)))
(define (named-let-exps exp) (map cadr (caddr exp)))
(define (named-let-extract-func exp)
  (list 'define
        (cons (named-let-name exp) (named-let-vars exp))
        (named-let-body exp)))

(define (let->combination-2 exp)
  (if (named-let? exp)
      (sequence->exp
       (list (named-let-extract-func exp)
             (cons (named-let-name exp) (named-let-exps exp))))
      (let->combination exp)))

; ex 4.9
; (do-while test? body)
; (let ((n 4) (lst '()))
;   (do-while (<= 0 n)
;     (set! lst (cons n lst))
;     (set! n (- n 1)))
;   lst) -> '(0 1 2 3 4)
(define (do-while? exp) (tagged-list? exp 'do-while))
(define (do-while-test exp) (cadr exp))
(define (do-while-body exp) (cddr exp))
(define (do-while->combination exp)
  (sequence->exp
   (list
    (list 'define (list 'iter)
          (make-if (do-while-test exp)
                   (sequence->exp
                    (append (do-while-body exp)
                            (list 'iter)))
                   'done))
    (list 'iter))))

; (do-for var (start stop [step]) body)
; (let ((lst '()))
;   (do-for i (1 5) (set! lst (cons i lst)))
;   lst) ; -> '(4 3 2 1)
(define (do-for? exp) (tagged-list? exp 'do-for))
(define (do-for-var exp) (cadr exp))
(define (do-for-body exp) (cdddr exp))
(define (do-for-grid exp) (caddr exp)) ; (start stop [step])
(define (do-for-start exp) (car (do-for-grid exp)))
(define (do-for-stop exp) (cadr (do-for-grid exp)))
(define (do-for-step exp)
  (if (null? (cddr (do-for-grid exp)))
      1
      (caddr (do-for-grid exp))))

(define (do-for->combination exp)
  (list 'let (list (list 'start (do-for-start exp))
                   (list 'stop (do-for-stop exp))
                   (list 'step (do-for-step exp)))
        (do-while->combination
         (list 'do-while
               '(<= start stop)
               (append (do-for-body exp)
                       '(set! start (+ start step)))))))

; testing of predicates
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))

; representing procedures
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


; represent an environment as a list of frames
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

; represent a frame as a pair of lists
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

; ex 4.11
; represent a frame as a list of bindings: ((var-1 val-1) ... (var-n val-n))
(define (make-frame-4.11 variables values)
  (map cons variables values))

(define (frame-variables-4.11 frame) (map car frame))
(define (frame-values-4.11 frame) (map cdr frame))
(define (add-binding-to-frame!-4.11 var val frame)
  (cons (cons var val) frame))

; operations on environments
(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (error "variables and values are not of same length" vars vals)))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? (car vals) '*unassigned*) ; ex 4.16
                 (error "the procedure is not assigned" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

; ex 4.12
; define env operation procedure in terms of a abstract procedure
(define (traverse-environment var null-action var-action env)
  (define (scan vars vals)
    (cond ((null? vars) (null-action env))
          ((eq? var (car vars)) (var-action vals))
          (else (scan (cdr vars) (cdr vals)))))
  (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values frame)))))

(define (set-variable-value!-4.12 var val env)
  (define (null-action env)
    (traverse-environment var null-action var-action (enclosing-environment env)))
  (define (var-action vals) (set-car! vals val))
  (traverse-environment var null-action var-action env))

(define (define-variable-value!-4.12 var val env)
  (define (null-action env)
    (add-binding-to-frame! var val (first-frame env)))
  (define (var-action vals) (set-car! vals val))
  (traverse-environment var null-action var-action env))

(define (lookup-variable-value-4.12 var env)
  (define (null-action env)
    (traverse-environment var null-action var-action (enclosing-environment env)))
  (define (var-action vals) (car vals))
  (traverse-environment var null-action var-action env))

; ex 4.13
; remove the binding (in the first frame) from the environment
(define (make-unbound! var env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (error "Unbound variable -- MAKE-UNBOUND!" var))
            ((eq? var (car vars))
             (set! vars (cdr vars))
             (set! vals (cdr vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame) (frame-values frame))))

; global env, containing primitive procedures and true/false
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

; primitive procedures
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'caddr caddr)
        (list 'cddr cddr)
        (list 'cdddr cdddr)
        (list 'caar caar)
        (list '+ +)
        (list '= =)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '< <)
        (list '> >)
        (list '>= >=)
        (list '<= <=)
        (list 'abs abs)
        (list 'append append)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'symbol? symbol?)
        (list 'cons cons) ;;; and more primitives
        (list 'null? null?)))

(define (primitive-procedure-names) (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))

; REPL (read-eval-print loop)
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

; avoid printing the environment of a compound procedure
(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))

; eval
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((and? exp) (eval (and->if exp) env))
        ((or? exp) (eval (or->if exp) env))
        ((let? exp) (eval (let->combination exp) env))
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((letrec? exp) (eval (letrec->let exp) env))
        ((do-while? exp) (eval (do-while->combination exp) env))
        ((do-for? exp) (eval (do-for->combination exp) env))
        ((application? exp) (i-apply (eval (operator exp) env) (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

(define apply-in-underlying-scheme apply) ; Scheme apply

; ex 4.3
; data directed style
(define (make-table)  "...")
(define (put row col truc) "...")
(define (get row col) "...")

(define eval-table (make-table))
(put 'eval 'quote (lambda (exp env) (text-of-quotation exp)))
(put 'eval 'set! eval-assignment)
(put 'eval 'define  eval-definition)
(put 'eval 'if eval-if)
(put 'eval 'lambda (lambda (exp env) (make-procedure (lambda-parameters exp) (lambda-body exp) env)))
(put 'eval 'begin (lambda (exp env) (eval-sequence (begin-actions exp) env)))
(put 'eval 'cond (lambda (exp env) (eval-data-directed (cond->if exp) env)))

(define (eval-data-directed exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((get 'eval (car exp)) ((get 'eval (car exp)) exp env))
        ((application? exp)
         (i-apply (eval (operator exp) env)
                  (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

; apply
(define (i-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else (error "Unknown procedure type -- APPLY" procedure))))

; ex 4.14
; using Scheme's map doesn't work, because in our metacircular evaluator procedures are represented
; differently than the underlying Scheme procedures. e.g., (map car (machin)), car will finally be
; evaluated to ('primitive car), because every primitive objects has been attached the tag 'primitive.
; But Scheme's map doesn't know ('primitive car). 

; ex 4.15
; halting problem: no algorithm can always decide if a program halts
; if (try try) halts -> (halts? try try) is true -> (try try) runs forever
; otherwise (halts? try try) is false -> (try try) halts
; this contractions shows that we cannot decide if (try try) halts or not.
(define (halts? proc) "not always work")
(define (run-forever) (run-forever))
(define (try p)
  (if (halts? p p)
      (run-forever)
      'halted))

; simultaneous scoping for internal definitions

; ex 4.16
(define (scan-out-defines proc-body)
  (define (let-part defines)
    (map (lambda (x) (list (definition-variable x) '*unassigned*)) defines))
  
  (define (set-part defines)
    (map (lambda (x) (list 'set! (definition-variable x) (definition-value x))) defines))
  
  (define (iter body def-acc others-acc)
    (cond ((null? body)
           (if (null? def-acc)
               proc-body
               (list (cons 'let
                           (cons (let-part def-acc)
                                 (append (set-part def-acc) others-acc))))))
          ((definition? (car body))
           (iter (cdr body) (append def-acc (list (car body))) others-acc))
          (else (iter (cdr body) def-acc (append others-acc (list (car body)))))))
  (iter proc-body '() '()))

(display (scan-out-defines '((define u e1) (define v e2) exp1 exp2))) (newline)
;;; ((let ((u *unassigned*) (v *unassigned*)) (set! u e1) (set! v e2) exp1 exp2))

; ex 4.17
;
; 1. define will add the binding of variables to the first frame, it won't create a new frame,
; the transformed program uses let, which will be firstly transformed to lambda expression
; then to compound procedure by the interpreter, and evaluating the compound procedure will
; create a new frame for the parameter-argument binding which extends the current env.
;
; 2. move internal definitions to the top of the body
(define (move-defs-to-top proc-body)
  (define (iter body def-acc others-acc)
    (cond ((null? body)
           (if (null? def-acc)
               proc-body
               (append def-acc others-acc)))
          ((definition? (car body))
           (iter (cdr body) (append def-acc (list (car body))) others-acc))
          (else (iter (cdr body) def-acc (append others-acc (list (car body)))))))
  (iter proc-body '() '()))

(display (move-defs-to-top '((define u e1) exp1 (define v e2) exp2))) (newline)
;; ((define u e1) (define v e2) exp1 exp2)

; ex 4.18
; 
; 1. It won't work, because when calling (b (stream-map f y)), y is *unassigned*.
;
; (define (solve f y0 dt)
;   (let ((y '*unassigned*)
;         (dy '*unassigned*))
;     (let ((a (integral (delay dy) y0 dt))
;           (b (stream-map f y)))
;       (set! y a)
;       (set! dy b))
;     y))
;
; 2. It will work.
;
; (define (solve f y0 dt)
;   (let ((y '*unassigned*)
;         (dy '*unassigned*))
;        (set! y (integral (delay dy) y0 dt))
;        (set! dy (stream-map f y))
;     y))


; ex 4.19
; If internal definitions should be simultaneous, then Eva is correct.
; But if it's hard to implement, we might as well report an error.
; (let ((a 1))
;   (define (f x)
;     (define b (+ a x))
;     (define a 5)
;     (+ a b))
;   (f 10)) ;; error : a undefined, cannot use before initialization


; ex 4.20
; a. (letrec ((var-1 exp-1) ... (var-n exp-n)) <body>)
(define (letrec? exp) (tagged-list? exp 'letrec))
(define letrec-bindings cadr)
(define letrec-body let-body)

(define (letrec->let exp)
  (define (let-part exp)
    (map (lambda (x) (list (car x) '*unassigned*)) (letrec-bindings exp)))
  (define (set-part exp)
    (map (lambda (x) (cons 'set! x)) (letrec-bindings exp)))
  (cons 'let
        (cons (let-part exp)
              (append (set-part exp)
                      (letrec-body exp)))))

(display (letrec->let '(letrec ((v1 e1) (v2 e2)) <body>))) (newline)
;; (let ((v1 *unassigned*) (v2 *unassigned*)) (set! v1 e1) (set! v2 e2) <body>)

; b. if we use let instead of letrec, even? and odd? will be defined in different envs,
; but they are mutually defined, so when defining even?, odd? is not defined yet.
(define (f x)
  (letrec ((even? (lambda (n)
                    (if (= n 0)
                        true
                        (odd? (dec n)))))
           (odd? (lambda (n)
                   (if (= n 0)
                       false
                       (even? (dec n))))))
    "rest of body of f"))

(letrec ((fac
          (lambda (n)
            (if (= n 1)
                1
                (* n (fac (- n 1)))))))
  (fac 10))

; ex 4.21
; a.
((lambda (n) ;;; factorial
   ((lambda (fact)
      (fact fact n))
    (lambda (ft k)
      (if (= k 1)
          1
          (* k (ft ft (- k 1))))))))

((lambda (n) ;;; fibonacci
   ((lambda (fib)
      (fib fib n))
    (lambda (fb k)
      (cond ((= k 0) 0)
            ((= k 1) 1)
            (else (+ (fb fb (- k 1))
                     (fb fb (- k 2))))))))
 10)

; b.
(define (even?-4.21-1 x)
  (define (even? n)
    (if (= n 0)
        true
        (odd? (dec n))))
  (define (odd? n)
    (if (= n 0)
        false
        (even? (dec n))))
  (even? x))

(define (even?-4.21-2 x)
  ((lambda (even? odd?)
     (even? even? odd? x))
   (lambda (ev? od? n)
     (if (= n 0) true (od? ev? od? (- n 1))))
   (lambda (ev? od? n)
     (if (= n 0) false (ev? ev? od? (- n 1))))))

(even?-4.21-1 5) ; #f
(even?-4.21-2 4) ; #t

; TEST
(define the-global-environment (setup-environment))
(driver-loop)

; separating syntactic analysis from execution
(define (better-eval exp env) ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp))) ; ex 4.22n
        ((application? exp) (analyze-application exp))
        (else (error "Unknown expression type: ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env) (set-variable-value! var (vproc env) env))))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env) (define-variable! var (vproc env) env))))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env) (if (true? (pproc env))
                      (cproc env)
                      (aproc env)))))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure vars bproc env))))

; * combine expressions in the sequence to produce a procedure that takes an env
; as arg and sequentially calls each individual procedure with the env as arg.
(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
       (fproc env)
       (map (lambda (aproc) (aproc env)) aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment
           (procedure-parameters proc)
           args
           (procedure-environment proc))))
        (else
         (error "Unknown procedure type: EXECUTE-APPLICATION" proc))))

; ex 4.23
; execute-sequence is not executed during syntactic analysis, so at runtime there's an
; additional overhead of testing the null? condition when sequencing those procedures.
; the version in the text combines the procedure sequence into a single procedure at
; analysis time, and there's no such overhead during runtime.
(define (analyze-sequence-bad exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs) (error "Empty sequence: ANALYZE"))
    (lambda (env) (execute-sequence procs env))))