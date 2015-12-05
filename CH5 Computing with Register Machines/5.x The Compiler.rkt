#lang planet neil/sicp

;; the compiler

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


;; open code primitives

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



;; compile-time environment operations

(define the-empty-compile-time-environment '())
(define (extend-compile-time-environment vars base-env) (cons vars base-env))


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
       (lambda-body exp)  ; ex 5.43
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