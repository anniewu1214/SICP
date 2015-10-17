#lang planet neil/sicp


; representation of complex numbers
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))

; rectangular form
(define real-part car)
(define imag-part cdr)

(define (square x) (* x x))
(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))
(define (angle z)
  (atan (real-part z) (imag-part z)))

(define make-from-real-imag cons)
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

; polar form
(define magnitude car)
(define angle cdr)

(define (real-part z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part z)
  (* (mangnitude z) (sin (angle z))))

(define make-from-mag-ang cons)
(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))


; tagged data, need to modify original implementation, violates the rule of encapsulation
(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

; modifiy the original implementation

; rectangular form
(define real-part-rectangular car)
(define imag-part-rectangular cdr)

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

; polar form
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define magnitude-polar car)
(define angle-polar cdr)

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar (cons (sqrt (+ (square x) (square y)))
                           (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

; selectors
(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknow type: REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknow type: IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknow type: MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknow type: ANGLE" z))))

(define make-from-real-imag make-from-real-imag-rectangular)
(define make-from-mag-ang make-from-mag-ang-polar)

; Data-Directed Programming and Additivity

; installs <item> in the table, indexed by <op> and <type>
(define put <op> <type> <item>)
; looks up the <op> <type> entry in the table
(define get <op> <type>)

(define (install-rectangular-package)
  ;; internal procedures
  (define real-part car)
  (define imag-part cdr)
  (define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z)))))
  (define (angle z)
    (atan (real-part z) (imag-part z)))
  (define make-from-real-imag cons)
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define magnitude car)
  (define angle cdr)
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define make-from-mag-ang cons)
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error "No method for these types: APPLY-GENERIC"
                 (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))

(define (imag-par z) (apply-generic 'imag-part z))

(define (magnitude z) (apply-generic 'magnitude z))

(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

; ex 2.73

; a. (get 'deriv operator) looks up the operator in the table. We can't assimulate
; number? and variable? because they are independent of operators.

; b. group sum and product derivation in packages
(define (make-sum a b)
    (cond ((eq? a 0) b)
          ((eq? b 0) a)
          ((and (number? a) (number? b)) (+ a b))
          (else (list '+ a b))))

(define (make-product a b)
    (cond ((or (eq? a 0) (eq? b 0)) 0)
          ((eq? a 1) b)
          ((eq? b 1) a)
          ((and (number? a) (number? b)) (* a b))
          (else '(* a b))))

(define (install-sum-deriv-package)
  (define addend car)
  (define augend cadr)
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
  
  (put 'deriv '+ deriv-sum)
  'done)

(define (install-product-deriv-package)
  (define multiplier car)
  (define multiplicand cadr)
  (define (deriv-product s v)
    (make-sum (make-product (deriv (multiplier s) v) (multiplicand s))
              (make-product (multiplier s) (deriv (multiplicand s) v))
              ))
  
  (put 'deriv '* deriv-product)
  'done)

; c. exponentiation differentiation package
(define (install-exponentiation-deriv-package)
  (define base car)
  (define exponent cadr)
  (define (make-exponentiation base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          ((=number? base 1) 1)
          (else (list '** base exponent))))
  (define (deriv-exp exp var)
    (make-product (exponent exp)
                  (make-product (make-exponentiation (base exp)
                                                     (make-sum (exponent exp) -1))
                                (deriv (base exp) var))))
  
  (put 'deriv '** deriv-exp)
  'done)

(define variable? symbol?)
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v2 v1)))
(define (=number? exp num) (and (number? exp) (= exp num)))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp) var))))

(define operator car)
(define operands cdr)

; d. we need only to swap arguments in the PUT procedure

; ex 2.75, message passing
(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude (sqrt (+ (square x) (square y)))))
          ((eq? op 'angle) (atan y x))
          (else (error "Unknown op: MAKE-FROM-REAL-IMAG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknow op: MAKE-FROM-MAG-ANG" op))))
  dispatch)

; ex 2.76
; 1. generic operation with explicit dispatch: need to modify both
; 2. data-directed style: appropriate when new types must often be added
; 3. message-passing style: appropriate when new operations must often be added