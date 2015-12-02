#lang planet neil/sicp

;; the compiler

(define (compile exp target linkage)
  (cond ((self-evaluating? exp) (compile-self-evaluating exp target linkage))
        ((quoted? exp) (compile-quoted exp target linkage))
        ((variable? exp) (compile-variable exp target linkage))
        ((assignment? exp) (compile-assignment exp target linkage))
        ((definition? exp) (compile-definition exp target linkage))
        ((if? exp) (compile-if exp target linkage))
        ((lambda? exp) (compile-lambda exp target linkage))
        ((begin? exp) (compile-sequence (begin-actions exp) target linkage))
        ((cond? exp) (compile (cond->if exp) target linkage))
        ((application? exp) (compile-application exp target linkage))
        (else (error "Unknown expression type -- COMPILE" exp))))


(define (make-instruction-sequence needs modifies statements) (list needs modifies statements))
(define (empty-instruction-sequence) (make-instruction-sequence '() '() '()))



;; ex 5.31
;
;  (f 'x 'y)        all saves and restores are superfluous
;  ((f) 'x 'y)      ditto, because 'x and 'y are independent of env
;  (f (g 'x) y)     save and restore proc, env and argl, because of (g 'x) and y
;  (f (g 'x) 'y)    save and restore proc and argl, because 'y is independent of env



;; ex 5.32
;
; a. ev-application and related blocks in the EC-evaluator are modified
;
; b. 1. testing special cases adds additional overhead
;    2. some optimizations of compiler can't be done by adding rules to the interpreter,
;       e.g., the interpreter would examine expressions everytime for dispatching.



;; compiling linkage code

(define (compile-linkage linkage)
  (cond ((eq? linkage 'return)
         (make-instruction-sequence
          '(continue) '()
          '((goto (reg continue)))))
        ((eq? linkage 'next) (empty-instruction-sequence))
        (else (make-instruction-sequence
               '() '()
               '((goto (label ,linkage)))))))


(define (end-with-linkage linkage instruction-sequence)
  (preserving '(continue)
              instruction-sequence
              (compile-linkage linkage)))



;; compiling simple expressions

(define (compile-self-evaluating exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence
                     '() (list target)
                     '((assign ,target (const ,exp))))))


(define (compile-quoted exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence
                     '() (list target)
                     '((assign ,target (const ,(text-of-quotation exp)))))))


(define (compile-variable exp target linkage)
  (end-with-linkage linkage
                    (make-instruction-sequence
                     '(env) (list target)
                     '((assign ,target
                               (op lookup-variable-value) (const ,exp) (reg env))))))


(define (compile-assignment exp target linkage)
  (let ((var (assignment-variable exp))
        (get-value-code
         (compile (assignment-value exp) 'val 'next)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence
                                   '(env val) (list target)
                                   '((perform (op set-variable-value!)
                                              (const ,var)
                                              (reg val)
                                              (reg env))
                                     (assign ,target (const ok))))))))


(define (compile-definition exp target linkage)
  (let ((var (definition-variable exp))
        (get-value-code
         (compile (definition-value exp) 'val 'next)))
    (end-with-linkage linkage
                      (preserving '(env)
                                  get-value-code
                                  (make-instruction-sequence
                                   '(env val) (list target)
                                   '((perform (op define-variable!)
                                              (const ,var)
                                              (reg val)
                                              (reg env))
                                     (assign ,target (const ok))))))))



;; generate labels

(define label-counter 0)

(define (new-label-number)
  (set! label-counter (+ 1 label-counter))
  label-counter)

(define (make-label name)
  (string->symbol
   (string->append (symbol->string name)
                   (number->string (new-label-number)))))



;; compiling conditional expressions

(define (compile-if exp target linkage)
  (let ((t-branch (make-label 'true-branch))
        (f-branch (make-label 'false-branch))
        (after-if (make-label 'after-if)))
    (let ((consequent-linkage
           (if (eq? linkage 'next) after-if linkage)))
      (let ((p-code (compile (if-predicate exp) 'val 'next))
            (c-code (compile (if-consequent exp) target consequent-linkage))
            (a-code (compile (if-alternative exp) target linkage)))
        (preserving '(env continue)
                    
                    p-code
                    
                    (append-instruction-sequences
                     (make-instruction-sequence
                      '(val) '()
                      '((test (op false?) (reg val))
                        (branch (label ,f-branch))))
                     
                     (parallel-instruction-sequence
                      (append-instruction-sequences t-branch c-code)
                      (append-instruction-sequences f-branch a-code))
                     
                     after-if))))))


;; compiling sequences

(define (compile-sequences seq target linkage)
  (if (last-exp? seq)
      (compile (first-exp seq) target linkage)
      (preserving '(env continue)
                  (compile (first-exp seq) target 'next)
                  (compile-sequences (rest-exps seq) target linkage))))



;; representing compiled procedures

(define (make-compiled-procedure entry env) (list 'compiled-procedure entry env))
(define (compiled-procedure? proc) (tagged-list? proc 'compiled-procedure))
(define (compiled-procedure-entry c-proc) (cadr c-proc))
(define (compiled-procedure-env c-proc) (caddr c-proc))



;; compiling lambda expressions

(define (compile-lambda exp target linkage)
  (let ((proc-entry (make-label 'entry))
        (after-lambda (make-label 'after-lambda)))
    (let ((lambda-linkage
           (if (eq? linkage 'next) after-lambda linkage)))
      (append-instruction-sequences
       
       (tack-on-instruction-sequence
        
        (end-with-linkage lambda-linkage
                          (make-instruction-sequence
                           '(env) (list target)
                           '((assign ,target
                                     (op make-compiled-procedure)
                                     (label ,proc-entry)
                                     (reg env)))))
        
        (compile-lambda-body exp proc-entry))
       
       after-lambda))))


(define (compile-lambda-body exp proc-entry)
  (let ((formals (lambda-parameters exp)))
    (append-instruction-sequences

     (make-instruction-sequence
      '(env proc argl) '(env)
      '(,proc-entry
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env (op extend-environment) (const ,formals) (reg argl) (reg env))))

     (compile-sequence (lambda-body exp) 'val 'return))))