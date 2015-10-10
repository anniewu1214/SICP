#lang planet neil/sicp

; helper procedures
(define (average a b) (/ (+ a b) 2.0))
(define (average3 a b c) (/ (+ a b c) 3.0))
(define (square x) (* x x))
(define (inc x) (+ 1 x))
(define (cube x) (* x x x))
(define (identity x) x)

; ex 2.17
(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))

(last-pair (list 1 2 3 4))

; ex 2.18
(define (reverse items)
  (if (null? (cdr items))
      (car items)
      (cons (reverse (cdr items)) (car items))))

(reverse (list 1 2 3 4))

; ex 2.19
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (null? coin-values)) 0)
        (else (+ (cc amount (cdr coin-values))
                 (cc (- amount (car coin-values)) coin-values)))))

(cc 100 us-coins)

; ex 2.20
(define (same-parity . x)
  (let ((first (car x)))
    (define (iter items)
      (cond ((null? items) nil)
            ((= (remainder (- (car items) first) 2) 0) (cons (car items) (iter (cdr items))))
            (else (iter (cdr items)))))
    (cons first (iter (cdr x)))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

; ex 2.21
(define (square-list items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list-2 items)
  (map square items))

(square-list (list 1 2 3 4))
(square-list-2 (list 1 2 3 4))

; ex 2.22
; the 1st procedure cons the cdr items in front of the first item
; the 2nd procedure cons a list to a value recursively

; ex 2.23
(define (for-each proc items)
  (define (helper items)
    (proc (car items))
    (for-each proc (cdr items)))
  (if (null? items)
      #t
      (helper items)))

(for-each (lambda (x) (newline) (display x))
          (list 1 2 3))

; ex 2.25
(define x1 (list 1 3 (list 5 7) 9))
(define x2 (list (list 7)))
(define x3 (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(cadr (cadr (cdr x1)))
(caar x2)
(cadr (cadr (cadr (cadr (cadr (cadr x3))))))

; ex 2.26
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ; (1 2 3 4 5 6)
(cons x y) ; ((1 2 3) 4 5 6) -> (mcons (mcons 1 (mcons 2 (mcons 3 '()))) (mcons 4 (mcons 5 (mcons 6 '()))))
(list x y) ; ((1 2 3) (4 5 6)) -> (mcons (mcons 1 (mcons 2 (mcons 3 '()))) (mcons (mcons 4 (mcons 5 (mcons 6 '()))) '()))

; ex 2.27
(define (deep-reverse items)
  (if (pair? items)
      (if (null? (cdr items)) ; case ((...))
          (deep-reverse (car items))
          (cons (deep-reverse (cdr items)) (deep-reverse (car items))))
      items))

(deep-reverse (list 1 (list (list 2 3) 4 (list 5 6)) 7))

; ex 2.28
(define (fringe tree) 
   ???)

(fringe (list (list 1 2) (list 3 4) 5))