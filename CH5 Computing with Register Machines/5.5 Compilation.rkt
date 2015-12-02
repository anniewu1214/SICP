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