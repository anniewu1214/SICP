#lang planet neil/sicp

; eval
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

(define apply-in-underlying-scheme apply) ; Scheme apply

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
           (list-of-customised-args arguments (procedure-parameter-tags procedure) env) ; changed
           (procedure-environment procedure))))
        (else (error "Unknown procedure type -- APPLY-CUSTOMISE" procedure))))

; representing procedures
(define (procedure-parameters-customise procedure)
  (map (lambda (x)
         (if (not (pair? x))
             x
             (car x)))
       (cadr procedure)))

(define (procedure-parameter-tags procedure)
  (map (lambda (x)
         (if (not (pair? x))
             'eager
             (cadr x)))
       (cadr procedure)))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps) env))))

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

; representing thunks
(define (lazy-memo? arg) (and (pair? arg) (eq? (cadr arg) 'lazy-memo)))
(define (lazy? arg) (and (pair? arg) (eq? (cadr arg) 'lazy)))

(define (actual-value exp env)
  (force-it (eval-customise exp env)))

(define (thunk? obj) (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj) (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (delay-it type exp env) (list type exp env))
(define (no-memo-thunk? obj) (tagged-list? obj 'no-memo-thunk))

(define (force-it obj)
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

; conditionals
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval-customise (if-consequent exp) env)
      (eval-customise (if-alternative exp) env)))

; sequences
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval-customise (first-exp exps) env))
        (else (actual-value (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

; assignments and definitions
(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval-customise (assignment-value exp) env)
                       env))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval-customise (definition-value exp) env)
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
(define input-prompt ";;; Customisable-Eval input:")
(define output-prompt ";;; Customisable-Eval value:")

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
        (list 'newline newline)
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

; TEST IN REPL
; (define count 0)
; (define (id x) (set! count (+ count 1)) x)
; (define (square x) (* x x))
; (define (square-lazy (x lazy)) (* x x))
; (define (square-lazy-memo (x lazy-memo)) (* x x))
; (square-lazy (id 10))
; count ; output: 2
; (square-lazy-memo (id 10))
; count ; output: 3