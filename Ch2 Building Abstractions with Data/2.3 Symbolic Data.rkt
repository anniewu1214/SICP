#lang planet neil/sicp

; ex 2.53
(define (memq item x)
  (cond ((null? x) #f)
        ((eq? (car x) item) x)
        (else (memq item (cdr x)))))

(list 'a 'b 'c) ; ('a 'b 'c)
(list (list 'george)) ; (('george))
(cdr '((x1 x2) (y1 y2))) ; (('y1 'y2))
(cadr '((x1 x2) (y1 y2))) ; ('y1 'y2)
(pair? (car '(a short list))) ; #f
(memq 'red '((red shoes) (blue socks))) ; (red shoes)
(memq 'red '(red shoes blue socks)) ; (read shoes blue socks)

; ex 2.54
(define (equal? l1 l2)
  (cond ((and (not (pair? l1)) (not (pair? l2))) (eq? l1 l2))
        ((and (pair? l1) (pair? l2)) (and (equal? (car l1) (car l2)) (equal? (cdr l1) (cdr l2))))
        (else #f)))

(equal? '(this (is a) list) '(this (is a) list))
(equal? '(this (is a) list) '(this is a list))

; ex 2.55
(car ''abracadabra) ; (car (quote (quote abracadabra))) -> quote

; e.g. Symbolic differentiation
(define variable? symbol?)

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define addend cadr)
(define augend caddr)

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define multiplier cadr)
(define multiplicand caddr)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (multiplicand exp)
                        (deriv (multiplier exp) var))))
        (else
         (error "unknow expression type -- DERIV" exp))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(display (deriv '(* (* x y) (+ x 3)) 'x))
(newline)
