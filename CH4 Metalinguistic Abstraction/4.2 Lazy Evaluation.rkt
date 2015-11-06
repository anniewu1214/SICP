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

