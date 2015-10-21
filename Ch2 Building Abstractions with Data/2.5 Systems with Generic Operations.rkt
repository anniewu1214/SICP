#lang planet neil/sicp
; put and get items on the table
(define (put <op> <type> <item>) nil)
(define (get <op> <type>) nil)

; ex 2.78, constructor and selectors of tag
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (if (number? datum)
          'scheme-number
          (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (if (number? datum)
          datum
          (error "Bad tagged datum: CONTENTS" datum))))

(define (square x) (* x x))

; complex number-rectangular representation
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

; complex-number-polar representation
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

; Generic Arithmetic Operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

; ordinary numbers
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number, scheme-number) (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number, scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number, scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number, scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  (put 'equ? '(scheme-number, scheme-number) =)
  (put '=zero? 'scheme-number (lambda (x) (= x 0)))
  'done)

(define (make-scheme-number n) ((get 'make 'scheme-number) n))

; rational numbers
(define (install-rational-package)
  ;; internal procedures
  (define (make-rat n d)
    (let ((g ((if (< d 0) - +) (gcd n d))))
      (cons (/ n g) (/ d g))))
  (define numer car)
  (define denom cdr)
  (define (print-rat x)
    (newline)
    (display (numer x)) (display "/") (display (denom x)))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer y) (denom y)) (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (= (* (numer x) (denom y)) (* (numer y) (denom x))))
  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? 'rational (lambda (x) (numer x) 0))
  'done)

; complex numbers
(define (install-complex-package)
  ;; imported procedures from rectangular and polar package
  (define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang x y) ((get 'make-from-mag-ang 'polar) x y))
  
  ;; internal procedures
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
  (define (equ? z1 z2)
    (and (= (real-part z1) (real-part z2)) (= (imag-part z1) (imag-part z2))))
  
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  ; ex .2.77
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  
  (put 'add '(complex complex) (lambda (x y) (tag (add-complex x y))))
  (put 'sub '(complex complex) (lambda (x y) (tag (sub-complex x y))))
  (put 'mul '(complex complex) (lambda (x y) (tag (mul-complex x y))))
  (put 'div '(complex complex) (lambda (x y) (tag (div-complex x y))))
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (x y) (tag (make-from-mag-ang x y))))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? 'complex (lambda (x) (= (real-part x) 0) (= (imag-part x) 0)))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-complex-from-mag-ang x y)
  ((get 'make-from-mag-ang 'polar) x y))

; ex 2.79
(define (equ? x y) (apply-generic 'equ? x y))
; ex 2.80
(define (=zero? x y) (apply-generic '=zero? x y))

; Combining Data of Different Types

; coercion table: transform type1->type2
(define (put-coercion <type1> <type2> <transform>) nil)
(define (get-coercion <type1> <type2>) nil)

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)




