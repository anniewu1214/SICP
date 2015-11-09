#lang planet neil/sicp

; ex 4.25
; applicative order: infinite recursion
; normal order: 5!
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))

(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

; ex 4.26
; if unless is a special form rather than a procedure, its args won't be evaluated eagerly
; but unless can't be used with higher-order procedures such as map, filter, fold, etc.
(define (unless->if exp env)
  (define unless-condition cadr)
  (define unless-usual caddr)
  (define unless-exceptional cadddr)
  (make-if (unless-condition exp)
           (unless-exceptional exp)
           (unless-usual exp))) 

; eval
(define (eval-lazy exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp env))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval-lazy (cond->if exp) env))
        ((application? exp)
         (apply-lazy (actual-value (operator exp) env)
                     (operands exp)
                     env))
        (else (error "Unknown expression type -- EVAL-LAZY" exp))))

(define apply-in-underlying-scheme apply) ; Scheme apply

; apply
(define (apply-lazy procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; primitive procs are strict
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env) ; compound procs are non-strict
           (procedure-environment procedure))))
        (else (error "Unknown procedure type -- APPLY-LAZY" procedure))))

; procedure arguments
(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps) env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps) env))))

; representing thunks
(define (actual-value exp env)
  (force-it (eval-lazy exp env)))

(define (delay-it exp env) (list 'thunk exp env))
(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)  ; replace exp with its value
           (set-cdr! (cdr obj) '())     ; forget unneeded environment
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

; conditionals
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval-lazy (if-consequent exp) env)
      (eval-lazy (if-alternative exp) env)))

; sequences
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval-lazy (first-exp exps) env))
        (else (eval-lazy (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; assignments and definitions
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval-lazy (assignment-value exp) env)
                       env))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval-lazy (definition-value exp) env)
    env))

; representing expressions
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

; (quote <text-of-quotation>)
(define (quoted? exp) (tagged-list? exp 'quote))

; ex 4.33
; (define (text-of-quotation exp) (cadr exp))
(define (text-of-quotation exp env)
  (let ((text (cadr exp)))
    (if (pair? text)
        (eval-lazy (make-list text) env)
        text)))

(define (make-list exp)
  (if (null? exp)
      '()
      (list 'cons (list 'quote (car exp)) (make-list (cdr exp)))))

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
(define (rest-exps seq) (cdr seq))
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

; derived expressions
; (cond ((p-1) actions-1) ... ((p-n) actions-n) (else actions-else))
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))

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
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))

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

; REPL (read-eval-print loop)
(define input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
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

; TEST
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
        (list 'display display)
        (list 'list list)
        (list 'cons cons) ;;; and more primitives
        (list 'null? null?)))

(define (primitive-procedure-names) (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme (primitive-implementation proc) args))

(define the-global-environment (setup-environment))
(driver-loop)
; (define (try a b) (if (= a 0) a b))
; (try 0 (/ 1 0))

; ex 4.27
; interactions between lazy eval and side effects can be very confusing
(define count 0)
(define (id x) (set! count (+ count 1)) x)
(define w (id (id 10))) ; (eval-lazy (id (id 10))), the outer id is called
;; count => 1, because (id 10) is delayed
;; w  => 10 
;; count => 2 (actual-value w), (id 10) is evaluated

; ex 4.28
(define (f g x) (g x))
(f id 10)
; id is delayed when passed to f, so (f id 10) -> (delay-it (id 10)), we need the
; actual procedure of the delayed proc to continue

; ex 4.29
; with memoization, the first time a thunk is forced, it stores the computed value
; subsequent forcings simply return the stored value without repeating the computation
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (+ (fib (- n 1))
           (fib (- n 2)))))

(define (square x) (* x x))
(square (id 10)) ; (id 10) is delayed when passed as argument
count
; with memoization the value of (id 10) will be stored, so it's called only 1 time
; without memoization, (id 10) will be called twice, so count is 2

; ex 4.30
; a. display is a primitive and it will force the evaluation of (car items)
(define (p1 x)
  (set! x (cons x '(2)))
  x)
(define (p2 x)
  (define (p e)
    e
    x)
  (p (set! x (cons x '(2)))))

(p1 1) ; (1 2)
(p2 1) ; 1, because (set! x (cons x '(2))) is delayed

; with the following modification, each exp in a sequence will be forced for evaluation
(define (eval-sequence-2 exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence-2 (rest-exps exps) env))))

; c. force-it will return the original object if it's not a thunk
; d. we should force the all expressions in the sequence except the final one, because
; that's the correct behavior of begin: evaluate all expressions, returns the last one

; ex 4.31
; upward-compatible extension of lazy evaluation
(define (list-of-customised-args exps types env)
  (define (process-arg arg type)
    (cond ((eq? type 'lazy-memo) (delay-it 'thunk arg env))
          ((eq? type 'lazy) (delay-it 'no-memo-thunk arg env))
          ((eq? type 'eager) (actual-value arg env))
          (else (error "Unknow parameter type " type))))
  (if (no-operands? exps)
      '()
      (cons (process-arg (first-operand exps) (car types))
            (list-of-customised-args (rest-operands exps) (cdr types) env))))

(define (procedure-parameters-customise procedure)
  (map (lambda (x)
         (if (not (pair? x))
             x
             (car x)))
       (cadr procedure)))

(define (procedure-parameter-types procedure)
  (map (lambda (x)
         (if (not (pair? x))
             'eager
             (cadr x)))
       (cadr procedure)))

(define (delay-it-customise type exp env) (list type exp env))
(define (no-memo-thunk? obj) (tagged-list? obj 'no-memo-thunk))

(define (force-it-customise obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result)
           (set-cdr! (cdr obj) '())
           result))
        ((no-memo-thunk? obj)
         (actual-value (thunk-exp obj) (thunk-env obj))) ; don't memoize
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))

(define (eval-customise exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval-customise (cond->if exp) env))
        ((application? exp)
         (apply-customise (actual-value (operator exp) env)
                          (operands exp)
                          env))
        (else (error "Unknown expression type -- EVAL-CUSTOMISE" exp))))

; apply
(define (apply-customise procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env)))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters-customise procedure)
           (list-of-customised-args arguments (procedure-parameter-types procedure) env) ; changed
           (procedure-environment procedure))))
        (else
         (error "Unknown procedure type -- APPLY-CUSTOMISE" procedure))))

; streams as lazy lists

; represents pairs as procedures
; define in REPL: 
(define (cons x y) (lambda (m) (m x y)))
(define (car z) (z (lambda (p q) p)))
(define (cdr z) (z (lambda (p q) q)))

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor)) items))

(define (add-lists l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        (else (cons (+ (car l1) (car l2))
                    (add-lists (cdr l1) (cdr l2))))))

; ex 4.32
; the car and cdr of a list is delayed, this permits us to create delayed versions of more general
; kinds of list structures such as lazy trees, not just sequences. Furthermore, all arguments to
; procedures are delayed uniformly, so we don't need to sprinkle our programs with explicat delay
; operations, such as the integral example, ex 3.77, and ex 3.79

; *ex 4.34, TODO
(define scheme-cons cons)
(define scheme-car car)
(define scheme-cdr cdr)
(define (cons x y) (scheme-cons 'cons (lambda (m) (m x y))))
(define (car z) ((scheme-cdr z) (lambda (p q) p)))
(define (cdr z) ((scheme-cdr z) (lambda (p q) q)))