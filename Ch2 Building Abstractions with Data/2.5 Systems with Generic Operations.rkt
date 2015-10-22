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
  (put 'neg 'scheme-number (lambda (x) (tag (- x))))
  (put 'sub '(scheme-number, scheme-number) (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number, scheme-number) (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number, scheme-number) (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  (put 'equ? '(scheme-number, scheme-number) =)
  (put 'project 'sheme-number round)
  (put '=zero? 'scheme-number (lambda (x) (= x 0)))
  (put 'raise 'scheme-number (lambda (x) (make-from-real-imag x 0)))
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
  (put 'neg 'rational (lambda (x) (make-rational (- (numer x)) (denom x))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? 'rational (lambda (x) (numer x) 0))
  (put 'raise 'rational
       (lambda (x)
         ((get 'div '(scheme-number, scheme-number)) (numer x) (denom x))))
  (put 'project 'rational
       (lambda (x)
         (make-scheme-number (round (/ (numer x) (denom x))))))
  'done)

(define (make-rational numer denom)
  ((get 'make 'rational) numer denom))

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
  (put 'neg 'complex (lambda (x) (make-from-real-imag (- (real-part x))
                                                      (- (imag-part x)))))
  (put 'mul '(complex complex) (lambda (x y) (tag (mul-complex x y))))
  (put 'div '(complex complex) (lambda (x y) (tag (div-complex x y))))
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (x y) (tag (make-from-mag-ang x y))))
  (put 'equ? '(complex complex) equ?)
  (put '=zero? 'complex (lambda (x) (= (real-part x) 0) (= (imag-part x) 0)))
  (put 'project 'complex (lambda (x) (real-part x)))
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

; coercion table: transform type1 to type2
(define (put-coercion <type1> <type2> <transform>) nil)
(define (get-coercion <type1> <type2>) nil)

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)

; ex 2.81
; a. we'll get into infinite recursion.
; b. apply-generic will work correctly as it is.
; c.
(define (apply-generic-coercion op . args)
  (define (error-msg type-tags)
    (error "No method for these types"(list op type-tags)))
  
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (equal? type1 type2)
                    (error-msg type-tags)
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2 (apply-generic-coercion op (t1->t2 a1) a2))
                            (t2->t1 (apply-generic-coercion op a1 (t2->t1 a2)))
                            (else (error-msg type-tags))))))
              (error-msg type-tags))))))


; ex 2.82
(define (apply-generic-general op . args)
  (define (coerce-items-to-type items type)
    (let ((t1->t2 (get-coercion (type-tag (car items)) type)))
      (if t1->t2
          (cons (t1->t2 (car items)) (coerce-items-to-type (cdr items) type))
          (cons (car items) (coerce-items-to-type (cdr items) type)))))
  
  (define (try-all-coercion acc)
    (if (null? acc)
        (error "No method for these types" (list op (map type-tag args)))
        (let ((coerced-items (coerce-items-to-type args (type-tag (car acc)))))
          (let ((proc (get op (map type-tag coerced-items))))
            (if proc
                (apply proc (map contents coerced-items))
                (try-all-coercion (cdr acc)))))))
  
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (try-all-coercion args)))))

; ex 2.83
; solution 1: use ('raise, <type>) table
(define (raise x) (apply-generic 'raise x))

; solution 2: use a coercion table
(put-coercion 'rational 'scheme-number
              (lambda (rat)
                ((get 'div '(scheme-number, scheme-number)) (car rat) (cadr rat))))

(put-coercion 'scheme-number 'complex
              (lambda (real)
                (make-complex-from-real-imag real 0)))

(define (raise-2 x)
  (define types '(rational scheme-number complex))
  (define (try acc)
    (if (< (length acc) 2)
        (error "Error raise type" (type-tag x))
        (if (eq? (type-tag x) (car acc))
            ((get-coercion (car acc) (cadr acc)) x)
            (try (cdr acc)))))
  (try types))

; ex 2.84
(define (apply-generic-raise op . args)

  (define (error-msg)
    (error "No method for these types" (list op (map type-tag args))))

  ; raise x to y
  (define (raise-args x y)
    (cond ((equal? x #f) #f)
          ((equal? (type-tag x) (type-tag y)) x)
          (else (raise-args (raise x) y))))
  
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((a1 (car args))
                    (a2 (cadr args)))
                (cond ((raise-args a1 a2)
                       (apply-generic-raise op (raise-args a1 a2) a2))
                      ((raise-args a2 a1)
                       (apply-generic-raise op a1 (raise-args a2 a1)))
                      (else error-msg)))
              error-msg)))))

; ex 2.85                 
(define (drop x)
  (let ((project-proc (get 'project (type-tag x))))
    (if project-proc
        (let ((project-number (project-proc (contents x))))
          (if (equ? (raise project-number) project-number)
              (drop project-number)
              x))
        x)))


; symbolic algebra

(define (install-polynomial-package)
  ;; internal procedures
  ; poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable poly) (car poly))
  (define (term-list poly) (cdr poly))

  ; ex 2.87
  (define (zero-poly? p)
    (define (zero-terms? termlist)
      (if (empty-termlist?)
          #t
          (and (=zero? (coeff (first-term termlist)))
               (zero-terms? (rest-terms termlist)))))
    (zero-terms? (term-list p)))
    
  ; terms
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (adjoin-term term term-list)
    (if (=zero? term)
        term-list
        (cons term term-list)))

  ; term operations
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1)) (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))

  (define (neg-terms L)
    (map (lambda (term) (make-term (order term) (- (coeff term))))
         L))
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        the-empty-termlist
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))

  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        the-empty-termlist
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))

  
  ; poly operations
  (define variable? symbol?)
  (define (same-variable? x y)
    (and (variable? x) (variable? y) (eq? x y)))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY" (list p1 p2))))

  (define (neg-poly p)
    (make-poly (variable p)
               (neg-terms p)))
  
  (define (sub-poly p1 p2)
    (add-poly p1 (neg-poly p2)))
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- MUL-POLY" (list p1 p2))))

  ; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial, polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  ; ex 2.88, negation and substraction
  (put 'neg 'polynomial
       (lambda (p) (tag (neg-poly p))))
  (put 'sub '(polynomial, polynomial)
       (lambda (p1 p2) (tag (sub-poly p1 p2))))
  (put 'mul '(polynomial, polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put 'zero 'polynomial zero-poly?)
  'done)

; ex 2.89, dense representation of term list