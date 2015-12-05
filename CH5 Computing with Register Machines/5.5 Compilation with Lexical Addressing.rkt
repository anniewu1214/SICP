#lang planet neil/sicp

;; the compiler using lexical addressing

(define (compile exp target linkage env)
  (cond ((self-evaluating? exp) (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp) (compile-variable exp target linkage env))
        ((assignment? exp) (compile-assignment exp target linkage env))
        ((definition? exp) (compile-definition exp target linkage env))
        ((if? exp) (compile-if exp target linkage env))
        ((lambda? exp) (compile-lambda exp target linkage env))
        ((begin? exp) (compile-sequence (begin-actions exp) target linkage env))
        ((cond? exp) (compile (cond->if exp) target linkage env))
        ((open-code? exp env) (compile-open-code exp target linkage env))
        ((application? exp) (compile-application exp target linkage env))
        (else (error "Unknown expression type -- COMPILE" exp))))




;; compile-time environment operations

(define the-empty-compile-time-environment '())
(define (extend-compile-time-environment vars base-env) (cons vars base-env))


;; ex 5.41
;; find the lexical address of a variable in the compile time environment
(define (find-variable var compile-time-env)
  (define (search-var-in-frame frame displacement)
    (cond ((null? frame) false)
          ((eq? var (car frame)) displacement)
          (else (search-var-in-frame (cdr frame) (+ 1 displacement)))))
  
  (define (search-var-in-env env frame-number)
    (if (null? env)
        'not-found
        (let ((displacement (search-var-in-frame (car env) 0)))
          (if displacement
              (list frame-number displacement)
              (search-var-in-env (cdr env) (+ 1 frame-number))))))
  
  (search-var-in-env compile-time-env 0))


;; test find-variable 
;(display (find-variable 'c '((y z) (a b c d e) (x y)))) ; (1 2)
;(display (find-variable 'x '((y z) (a b c d e) (x y)))) ; (2 0)
;(display (find-variable 'w '((y z) (a b c d e) (x y)))) ; 'not-found




;; ex 5.39
;; lexical address operations

(define (lexical-address frame displacement) (cons frame displacement))
(define (frame-number address) (car address))
(define (displacement-number address) (cdr address))


(define (lexical-address-lookup address env)
  (let* ((frame (list-ref env (frame-number address)))
         (value (list-ref (frame-values frame) (displacement-number address))))
    (if (eq? value '*unassigned*)
        (error "variable unassigned -- LEXICAL-ADDRESS-LOOKUP" address)
        value)))


(define (list-set! list i value)
  (if (= i 0)
      (set-car! list value)
      (list-set! (cdr list) (- i 1) value)))

(define (lexical-address-set! address value env)
  (let ((frame (list-ref env (frame-number address))))
    (list-set! frame (displacement-number address) value)))



;; combining instruction sequences

(define (make-instruction-sequence needs modifies statements) (list needs modifies statements))
(define (empty-instruction-sequence) (make-instruction-sequence '() '() '()))

(define (registers-needed s) (if (symbol? s) '() (car s)))
(define (registers-modified s) (if (symbol? s) '() (cadr s)))
(define (statements s) (if (symbol? s) (list s) (caddr s)))

(define (memq item x)
  (cond ((null? x) #f)
        ((eq? (car x) item) x)
        (else (memq item (cdr x)))))

(define (needs-register? seq reg) (memq reg (registers-needed seq)))
(define (modifies-register? seq reg) (memq reg (registers-modified seq)))


;; appending instructions sequences
(define (append-instruction-sequences . seqs)
  (define (append-2-sequences seq1 seq2)
    (make-instruction-sequence
     
     (list-union (registers-needed seq1)
                 (list-difference (registers-needed seq2)
                                  (registers-modified seq1)))
     
     (list-union (registers-modified seq1)
                 (registers-modified seq2))
     
     (append (statements seq1) (statements seq2))))
  (define (append-seq-list seqs)
    (if (null? seqs)
        (empty-instruction-sequence)
        (append-2-sequences (car seqs)
                            (append-seq-list (cdr seqs)))))
  (append-seq-list seqs))


(define (list-union s1 s2)
  (cond ((null? s1) s2)
        ((memq (car s1) s2) (list-union (cdr s1) s2))
        (else (cons (car s1) (list-union (cdr s1) s2)))))

(define (list-difference s1 s2)
  (cond ((null? s1) '())
        ((memq (car s1) s2) (list-difference (cdr s1) s2))
        (else (cons (car s1) (list-difference (cdr s1) s2)))))


;; preserving
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-instruction-sequences seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (needs-register? seq2 first-reg)
                 (modifies-register? seq1 first-reg))
            (preserving (cdr regs)
                        
                        (make-instruction-sequence
                         (list-union (list first-reg) (registers-needed seq1))
                         (list-difference (registers-modified seq1) (list first-reg))
                         (append `((save ,first-reg))
                                 (statements seq1)
                                 `((restore ,first-reg))))
                        
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))


;; append procedure body to another sequence
(define (tack-on-instruction-sequence seq body-seq)
  (make-instruction-sequence
   (registers-needed seq)
   (registers-modified seq)
   (append (statements seq) (statements body-seq))))


;; append two alternative branchs following a test
(define (parallel-instruction-sequences seq1 seq2)
  (make-instruction-sequence
   (list-union (registers-needed seq1)
               (registers-needed seq2))
   (list-union (registers-modified seq1)
               (registers-modified seq2))
   (append (statements seq1) (statements seq2))))


;; compiling linkage code

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence
          '(continue) '()
          '((goto (reg continue)))))
        ((eq? linkage 'next) (empty-instruction-sequence))
        (else (make-instruction-sequence
               '() '()
               `((goto (label ,linkage)))))))


(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))



;; compiling simple expressions

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence
                     '() (list target)
                     `((assign ,target (const ,exp))))))


(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence
                     '() (list target)
                     `((assign ,target (const ,(text-of-quotation exp)))))))


;; ex 5.42
(define (compile-variable exp target linkage env)
  (let ((address (find-variable exp env)))
    (end-with-linkage
     linkage
     (make-instruction-sequence
      '(env) (list target)
      (if (eq? address 'not-found)
          `((assign ,target (op lookup-variable-value) (const ,exp) (reg env)))
          `((assign ,target (op lexical-address-lookup) (const ,address) (reg env))))))))



(define (compile-assignment exp target linkage env)
  (let ((var (assignment-variable exp))
        (get-value-code (compile (assignment-value exp) 'val 'next env)))
    (let ((address (find-variable var env)))
      (end-with-linkage
       linkage
       (preserving
        '(env)
        get-value-code
        (make-instruction-sequence
         '(env val) (list target)
         (if (eq? address 'not-found)
             `((perform (op set-variable-value!) (const ,var) (reg val) (reg env))
               (assign ,target (const ok)))
             `((perform (op lexical-address-set!) (const ,address) (reg val) (reg env))
               (assign ,target (const ok))))))))))


(define (compile-definition exp target linkage env)
  (let ((var (definition-variable exp))
        (get-value-code (compile (definition-value exp) 'val 'next env)))
    (end-with-linkage
     linkage
     (preserving '(env)
                 get-value-code
                 (make-instruction-sequence
                  '(env val) (list target)
                  `((perform (op define-variable!) (const ,var) (reg val) (reg env))
                    (assign ,target (const ok))))))))



;; generate labels

(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
   (string-append (symbol->string name)
                  (number->string (new-label-number)))))



;; compiling conditional expressions

(define (compile-if exp target linkage env)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))                    
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next env))
            (c-code (compile (if-consequent exp) target consequent-linkage env))
            (a-code (compile (if-alternative exp) target linkage env)))
        (preserving '(env continue)
                    
                    ; <seq 1>
                    p-code
                    
                    ; <seq 2>
                    (append-instruction-sequences
                     ; <seq 2.1>
                     (make-instruction-sequence
                      '(val) '()
                      `((test (op false?) (reg val))
                        (branch (label ,f-branch))))
                     ; <seq 2.2>
                     (parallel-instruction-sequences
                      (append-instruction-sequences t-branch c-code)
                      (append-instruction-sequences f-branch a-code))
                     ; <seq 2.3>
                     after-if))))))


;; compiling sequences

(define (compile-sequence seq target linkage env)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage env)
      (preserving '(env continue)
                  (compile (first-exp seq) target 'next env)
                  (compile-sequence (rest-exps seq) target linkage env))))



;; representing compiled procedures

(define (make-compiled-procedure entry env) (list 'compiled-procedure entry env))
(define (compiled-procedure? proc) (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))



;; compiling lambda expressions

(define (compile-lambda exp target linkage env)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       
       (tack-on-instruction-sequence
        
        (end-with-linkage lambda-linkage
                          (make-instruction-sequence
                           '(env) (list target)
                           `((assign ,target (op make-compiled-procedure) (label ,proc-entry) (reg env)))))
        
        (compile-lambda-body exp proc-entry env))
       
       after-lambda))))


;; simultaneous scoping for internal definitions
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


(define (compile-lambda-body exp proc-entry env)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences
     (make-instruction-sequence
      '(env proc argl) '(env)
      `(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env (op extend-environment) (const ,formals) (reg argl) (reg env))))
     (compile-sequence
      (scan-out-defines (lambda-body exp))  ; ex 5.43
      'val 'return
      (extend-compile-time-environment formals env)))))



;; compiling applications

(define (compile-application exp target linkage env)
  (let ((proc-code (compile (operator exp) 'proc 'next env))
        (operand-codes (map (lambda (operand) (compile operand 'val 'next env))
                            (operands exp))))
    (preserving '(env continue)
                proc-code
                (preserving '(proc continue)
                            (construct-arglist operand-codes)
                            (compile-procedure-call target linkage env)))))


;; compiling operands and construct argument list

(define (construct-arglist operand-codes)
  (let ((operand-codes (reverse operand-codes)))
    (if (null? operand-codes)
        (make-instruction-sequence
         '() '(argl)
         '((assign argl (const ()))))
        (let ((code-to-get-last-arg
               (append-instruction-sequences
                (car operand-codes)
                (make-instruction-sequence
                 '(val) '(argl)
                 '((assign argl (op list) (reg val)))))))
          (if (null? (cdr operand-codes))
              code-to-get-last-arg
              (preserving '(env)
                          code-to-get-last-arg
                          (code-to-get-rest-args (cdr operand-codes))))))))


(define (code-to-get-rest-args operand-codes)
  (let ((code-for-next-arg
         (preserving '(argl)
                     (car operand-codes)
                     (make-instruction-sequence
                      '(val argl) '(argl)
                      '((assign argl (op cons) (reg val) (reg argl)))))))
    (if (null? (cdr operand-codes))
        code-for-next-arg
        (preserving '(env)
                    code-for-next-arg
                    (code-to-get-rest-args (cdr operand-codes))))))



;; compiling procedure calls

(define (compile-procedure-call target linkage env)
  (let ((primitive-branch (make-label 'primitive-branch))
        (compiled-branch (make-label 'compiled-branch))
        (after-call (make-label 'after-call)))
    (let ((compiled-linkage (if (eq? linkage 'next) after-call linkage)))
      (append-instruction-sequences
       
       ; <seq 1>
       (make-instruction-sequence
        '(proc) '()
        `((test (op primitive-procedure?) (reg proc))
          (branch (label ,primitive-branch))))
       
       ; <seq 2>
       (parallel-instruction-sequences
        
        ; <seq 2.1>
        (append-instruction-sequences
         compiled-branch
         (compile-proc-appl target compiled-linkage env))
        
        ; <seq 2.2>
        (append-instruction-sequences
         primitive-branch
         (end-with-linkage linkage
                           (make-instruction-sequence
                            '(proc argl)
                            (list target)
                            `((assign ,target (op apply-primitive-procedure) (reg proc) (reg argl)))))))
       
       ; <seq 3>
       after-call))))



;; compiling compiled procedures

(define all-regs '(env proc val argl continue arg1 arg2))

(define (compile-proc-appl target linkage env)
  (cond ((and (eq? target 'val) (not (eq? linkage 'return)))
         (make-instruction-sequence
          '(proc) all-regs
          `((assign continue (label ,linkage))
            (assign val (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (not (eq? linkage 'return)))
         (let ((proc-return (make-label 'proc-return)))
           (make-instruction-sequence
            '(proc) all-regs
            `((assign continue (label ,proc-return))
              (assign val (op compiled-procedure-entry) (reg proc))
              (goto (reg val))
              ,proc-return
              (assign ,target (reg val))
              (goto (label ,linkage))))))
        ((and (eq? target 'val) (eq? linkage 'return))
         (make-instruction-sequence
          '(proc continue) all-regs
          '((assign val (op compiled-procedure-entry) (reg proc))
            (goto (reg val)))))
        ((and (not (eq? target 'val)) (eq? linkage 'return))
         (error "return linkage, target not val -- COMPILE" target))))






;; test

(display (compile
          '((lambda (x y)
              (lambda (a b c d e)
                ((lambda (y z) (* x y z))
                 (* a b x)
                 (+ c d x))))
            3
            4)
          'val
          'next
          the-empty-compile-time-environment))

(set! label-number 0)
(newline)
(newline)

;; compilation result

(define compiled-lambda-expression
  '(
    ; construct the procedure body and skip over code for the procedure body
    (assign proc (op make-compiled-procedure) (label entry1) (reg env))
    (goto (label after-lambda2))
    
    entry1
    (assign env (op compiled-procedure-env) (reg proc))
    ; (x y)
    (assign env (op extend-environment) (const (x y)) (reg argl) (reg env))
    ; construct the procedure body and skip over code for the procedure body
    (assign val (op make-compiled-procedure) (label entry3) (reg env))
    (goto (reg continue))
    
    entry3
    (assign env (op compiled-procedure-env) (reg proc))
    ; (a b c d e)
    (assign env (op extend-environment) (const (a b c d e)) (reg argl) (reg env))
    ; construct the procedure body and skip over code for the procedure body
    (assign proc (op make-compiled-procedure) (label entry5) (reg env))
    (goto (label after-lambda6))
    
    entry5
    (assign env (op compiled-procedure-env) (reg proc))
    ; (y z)
    (assign env (op extend-environment) (const (y z)) (reg argl) (reg env))
    ; compile (* x y z)
    (assign proc (op lookup-variable-value) (const *) (reg env))
    ; look up variable values using lexical addressing (at compile time)
    (assign val (op lexical-address-lookup) (const (0 1)) (reg env)) ; z
    (assign argl (op list) (reg val))
    (assign val (op lexical-address-lookup) (const (0 0)) (reg env)) ; y
    (assign argl (op cons) (reg val) (reg argl))
    (assign val (op lexical-address-lookup) (const (2 0)) (reg env)) ; x
    (assign argl (op cons) (reg val) (reg argl))
    
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch7))
    
    compiled-branch8
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    
    primitive-branch7
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (goto (reg continue))
    
    after-call9
    after-lambda6
    
    (save continue)
    (save proc)
    (save env)
    ; compile (+ c d x)
    (assign proc (op lookup-variable-value) (const +) (reg env))
    (assign val (op lexical-address-lookup) (const (1 0)) (reg env)) ; x
    (assign argl (op list) (reg val))
    (assign val (op lexical-address-lookup) (const (0 3)) (reg env)) ; d
    (assign argl (op cons) (reg val) (reg argl))
    (assign val (op lexical-address-lookup) (const (0 2)) (reg env)) ; c
    (assign argl (op cons) (reg val) (reg argl))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch13))
    
    compiled-branch14
    (assign continue (label after-call15))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    
    primitive-branch13
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    
    after-call15
    (assign argl (op list) (reg val))
    (restore env)
    (save argl)
    ; compile (* a b x)
    (assign proc (op lookup-variable-value) (const *) (reg env))
    (assign val (op lexical-address-lookup) (const (1 0)) (reg env)) ; x
    (assign argl (op list) (reg val))
    (assign val (op lexical-address-lookup) (const (0 1)) (reg env)) ; b
    (assign argl (op cons) (reg val) (reg argl))
    (assign val (op lexical-address-lookup) (const (0 0)) (reg env)) ; a
    (assign argl (op cons) (reg val) (reg argl))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch10))
    
    compiled-branch11
    (assign continue (label after-call12))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    
    primitive-branch10
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    
    after-call12
    (restore argl)
    (assign argl (op cons) (reg val) (reg argl))
    (restore proc)
    (restore continue)
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch16))
    
    compiled-branch17
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    
    primitive-branch16
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    (goto (reg continue))
    
    after-call18
    after-lambda4
    after-lambda2
    
    (assign val (const 4))
    (assign argl (op list) (reg val))
    (assign val (const 3))
    (assign argl (op cons) (reg val) (reg argl))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch19))
    
    compiled-branch20
    (assign continue (label after-call21))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))
    
    primitive-branch19
    (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
    
    after-call21))




;; ex 5.38
;; open code primitives

; (+ 1 2 3)
;
; (assign arg1 (const 1))
; (assign arg2 (const 2))
; (assign arg1 (op +) (reg arg1) (reg arg2))
; (assign arg2 (const 3))
; (assign val (op +) (reg arg1) (reg arg2))

;; ex 5.44
;; consult the compile-time envirinment to correctly compile expressions
;; involving the names of primitives
(define (open-code? exp env)
  (and (pair? exp)
       (memq (car exp) '(+ * - =))
       (eq? (find-variable (car exp) env) 'not-found)))


(define (compile-open-code exp target linkage env)
  (let ((operator (car exp))
        (first-operand (cadr exp))
        (rest-operands (cddr exp)))
    (preserving '(env continue)
                (compile first-operand 'arg1 'next env)
                (compile-open-code-rest-args operator rest-operands target linkage env))))

(define (compile-open-code-rest-args operator rest-operands target linkage env)
  (if (null? (cdr rest-operands))
      (preserving '(arg1 continue)
                  (compile (car rest-operands) 'arg2 'next env)
                  (end-with-linkage linkage
                                    (make-instruction-sequence
                                     '(arg1 arg2) (list target)
                                     `((assign ,target (op ,operator) (reg arg1) (reg arg2))))))
      
      (preserving '(env continue)
                  (preserving '(arg1)
                              (compile (car rest-operands) 'arg2 'next env)
                              (make-instruction-sequence
                               '(arg1 arg2) '(arg1)
                               `((assign arg1 (op ,operator) (reg arg1) (reg arg2)))))
                  (compile-open-code-rest-args operator (cdr rest-operands) target linkage env))))




;; compilation with primitives open coded

(display (compile
          '(define (factorial n)
             (if (= n 1)
                 1
                 (* (factorial (- n 1)) n)))
          'val
          'next
          the-empty-compile-time-environment))


(define (compiled-factorial-with-primitives-open-coded)
  '(
    (assign val (op make-compiled-procedure) (label entry1) (reg env))
    (goto (label after-lambda2))

    entry1
    (assign env (op compiled-procedure-env) (reg proc))
    (assign env (op extend-environment) (const (n)) (reg argl) (reg env))

    ; optimized
    (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
    (assign arg2 (const 1))
    (assign val (op =) (reg arg1) (reg arg2))

    (test (op false?) (reg val))
    (branch (label false-branch4))

    true-branch3
    (assign val (const 1))
    (goto (reg continue))

    false-branch4
    (save continue)
    (save env)

    ; optimized
    (assign proc (op lookup-variable-value) (const factorial) (reg env))
    (assign arg1 (op lexical-address-lookup) (const (0 0)) (reg env))
    (assign arg2 (const 1))
    (assign val (op -) (reg arg1) (reg arg2))
    (assign argl (op list) (reg val))
    (test (op primitive-procedure?) (reg proc))
    (branch (label primitive-branch6))

    compiled-branch7
    (assign continue (label proc-return9))
    (assign val (op compiled-procedure-entry) (reg proc))
    (goto (reg val))

    proc-return9
    (assign arg1 (reg val))
    (goto (label after-call8))

    primitive-branch6
    (assign arg1 (op apply-primitive-procedure) (reg proc) (reg argl))

    after-call8
    (restore env)
    (restore continue)
    ; optimized
    (assign arg2 (op lexical-address-lookup) (const (0 0)) (reg env))
    (assign val (op *) (reg arg1) (reg arg2))
    (goto (reg continue))

    after-if5
    after-lambda2

    (perform (op define-variable!) (const factorial) (reg val) (reg env))
    (assign val (const ok))))