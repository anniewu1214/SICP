#lang planet neil/sicp

;; primitive register machine operations

;; representing expressions
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))


;; (quote <text-of-quotation>)
(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))
;(define (tagged-list? exp tag) (if (pair? exp) (eq? (car exp) tag) false))


;; (define <var> <val>)
;; (define (<var> <par1> ... <parn>) <body>)
(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)
                   (cddr exp))))


;; (set! <var> <val>)
(define (assignment? exp) (tagged-list? exp 'set!)) 
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))


;; (if predicate consequent alternative)
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))
(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))


;; condition
(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))

;; ex 5.24
(define cond-first-clause-predicate caar)
(define cond-first-clause-actions cdar)
(define cond-rest-clauses cdr)

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last -- COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


;; let
(define (let? exp) (tagged-list? exp 'let))
(define (let-vars exp) (map car (cadr exp)))
(define (let-exps exp) (map cadr (cadr exp)))
(define (let-body exp) (cddr exp))
(define (let->combination exp)
  (cons (make-lambda (let-vars exp) (let-body exp)) (let-exps exp)))


;; (lambda (p1 ... p2) <body>)
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))
(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))


;; (begin e1 e2 ... en)
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


;; procedure application, (operator operand-1 ... operand-n)
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


;; testing of predicates
(define (true? x) (not (eq? x false)))
(define (false? x) (eq? x false))


;; representing procedures
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


;; represent an environment as a list of frames
(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())


;; represent a frame as a pair of lists
(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))


;; operations on environments
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


;; primitive procedures
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cadr cadr)
        (list 'cddr cddr)
        (list 'caar caar)
        (list '+ +)
        (list '= =)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '< <)
        (list '> >)
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
  (apply (primitive-implementation proc) args))


;; global env
(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define the-global-environment (setup-environment))
(define (get-global-environment) the-global-environment)
(define (reset-the-global-environment!)
  (set! the-global-environment (setup-environment)))


;; REPL
(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display (list 'compound-procedure
                     (procedure-parameters object)
                     (procedure-body object)
                     '<procedure-env>))
      (display object)))


;; supplementary
(define (empty-arglist) '())
(define (adjoin-arg arg arglist) (append arglist (list arg)))
(define (last-operand? ops) (null? (cdr ops)))



;;;;; procedure for lazy evaluation

 
;; list of all register machine operations (implemented as Scheme procedures)
(define eceval-operations
  (list
   ;; simple expressions
   (list 'self-evaluating? self-evaluating?)
   (list 'variable? variable?)
   (list 'quoted? quoted?)
   (list 'text-of-quotation text-of-quotation)
   
   ;; assignment
   (list 'assignment? assignment?)
   (list 'assignment-value assignment-value)
   (list 'assignment-variable assignment-variable)
   
   ;; definition
   (list 'definition? definition?)
   (list 'definition-variable definition-variable)
   (list 'definition-value definition-value)
   
   ;; if
   (list 'if? if?)
   (list 'if-predicate if-predicate)
   (list 'if-consequent if-consequent)
   (list 'if-alternative if-alternative)
   
   ;; cond
   (list 'cond? cond?)
   (list 'cond->if cond->if)
   (list 'cond-clauses cond-clauses)
   (list 'cond-first-clause-predicate cond-first-clause-predicate)
   (list 'cond-first-clause-actions cond-first-clause-actions)
   (list 'cond-else-clause? cond-else-clause?)
   (list 'cond-rest-clauses cond-rest-clauses)

   ;; let
   (list 'let? let?)
   (list 'let->combination let->combination)
   
   ;; lambda
   (list 'lambda? lambda?)
   (list 'lambda-parameters lambda-parameters)
   (list 'lambda-body lambda-body)
   
   ;; begin
   (list 'begin? begin?)
   (list 'begin-actions begin-actions)
   (list 'first-exp first-exp)
   (list 'last-exp? last-exp?)
   (list 'rest-exps rest-exps)
   
   ;; application
   (list 'application? application?)
   (list 'operands operands)
   (list 'operator operator)
   (list 'no-operands? no-operands?)
   (list 'first-operand first-operand)
   (list 'rest-operands rest-operands)
   
   ;; environment
   (list 'lookup-variable-value lookup-variable-value)
   (list 'define-variable! define-variable!)
   (list 'set-variable-value! set-variable-value!)
   (list 'extend-environment extend-environment)
   
   ;; procedures
   (list 'primitive-procedure? primitive-procedure?)
   (list 'apply-primitive-procedure apply-primitive-procedure)
   (list 'compound-procedure? compound-procedure?)
   (list 'procedure-parameters procedure-parameters)
   (list 'procedure-environment procedure-environment)
   (list 'procedure-body procedure-body)       
   (list 'make-procedure make-procedure)
   
   ;; REPL
   (list 'true? true?)
   (list 'prompt-for-input prompt-for-input)
   (list 'read read)
   (list 'get-global-environment get-global-environment)
   (list 'announce-output announce-output)
   (list 'user-print user-print)
   
   ;; supplementary
   (list 'empty-arglist empty-arglist)
   (list 'adjoin-arg adjoin-arg)
   (list 'last-operand? last-operand?)
   ))