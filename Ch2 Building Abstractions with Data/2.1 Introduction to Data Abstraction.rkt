#lang planet neil/sicp

(define (average a b) (/ (+ a b) 2.0))
(define (average3 a b c) (/ (+ a b c) 3.0))
(define (square x) (* x x))
(define (inc x) (+ 1 x))
(define (cube x) (* x x x))
(define (identity x) x)

; e.g. Arithmetic operation for rational numbers
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

; ex 2.1
(print-rat (make-rat 2 4)) ; 1/2 
(print-rat (make-rat -2 4)) ; -1/2 
(print-rat (make-rat 2 -4)) ; -1/2 
(print-rat (make-rat -2 -4)) ; 1/2

; ex 2.2, representation of line segments in a plane
(define make-point cons)
(define x-point car)
(define y-point cdr)
(define (print-point p)
  (newline)
  (display "(") (display (x-point p)) (display ",") (display (y-point p)) (display ")"))

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (midpoint-segment segment)
  (let ((s (start-segment segment))
        (e (end-segment segment)))
    (make-point (average (x-point s) (x-point e))
                (average (y-point s) (y-point e)))))

(print-point
 (midpoint-segment
  (make-segment (make-point 1 2)
                (make-point 3 4))))

; ex 2.3 representation of rectangles in a plane
(define (make-rect bottom-left top-right)
  (cons bottom-left top-right))

(define bottom-left car)
(define top-right cdr)
(define (top-left rect)
  (make-point (x-point (bottom-left rect))
              (y-point (top-right rect))))
(define (bottom-right rect)
  (make-point (x-point (top-right rect))
              (y-point (bottom-left rect))))

(define (width-rect rect)
  (abs (- (x-point (bottom-left rect))
          (x-point (bottom-right rect)))))
(define (height-rect rect)
  (abs (- (y-point (bottom-left rect))
          (y-point (top-left rect)))))

(define (area-rect rect)
  (* (width-rect rect) (height-rect rect)))
(define (perimeter-rect rect)
  (* (+ (width-rect rect) (height-rect rect)) 2))

(define r (make-rect (make-point 0 0)
                     (make-point 2 2)))
(area-rect r)
(perimeter-rect r)

; e.g. define compound data using procedures
(define (cons-1 x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m))))
  dispatch)
(define (car-1 z) (z 0))
(define (cdr-1 z) (z 1))

(car-1 (cons-1 1 2))
(cdr-1 (cons-1 1 2))

; ex 2.4
(define (cons-2 x y)
  (lambda (m) (m x y)))
(define (car-2 z)
  (z (lambda (p q) p)))
(define (cdr-2 z)
  (z (lambda (p q) q)))

(car-2 (cons-2 1 2))
(cdr-2 (cons-2 1 2))

; ex 2.5
(define (pow b n) ; b^n
  (define (gather b n acc)
    (cond ((= n 0) acc)
          ((even? n) (gather (square b) (/ n 2) acc))
          (else (gather b (- n 1) (* acc b)))))
  
  (gather b n 1))

(define (cons-3 x y)
  (* (pow 2 x) (pow 3 y)))
(define (car-3 z)
  (define (iter z result)
    (if (not (= (remainder z 2) 0))
        result
        (iter (/ z 2) (inc result))))
  (iter z 0))
(define (cdr-3 z)
  (define (iter z result)
    (if (not (= (remainder z 3) 0))
        result
        (iter (/ z 3) (inc result))))
  (iter z 0))

(car-3 (cons-3 5 2))
(cdr-3 (cons-3 5 2))

; ex 2.6, Church numerals
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))

; ex 2.7 Iterval Arithmetic
(define make-interval cons)
(define lower-bound car)
(define upper-bound cdr)

(define i (make-interval 1 3))
(define j (make-interval 2 5))

(define (print-interval i)
  (newline)
  (display "[") (display (lower-bound i)) (display ",") (display (upper-bound i)) (display "]"))

(print-interval i)
(print-interval j)

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(print-interval (add-interval i j))

; ex 2.8
(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

(print-interval (sub-interval i j))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(print-interval (mul-interval i j))

; ex 2.9, [0, 2] * [0, 2] = [0, 4], w1 = 1, w2 = 1, w1*w2 != 2

; ex 2.10, y should not spans 0
(define (div-interval x y)
  (if (<= (* (lower-bound y) (upper-bound y)) 0)
      (error "interval spans 0" (print-interval y))
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(print-interval (div-interval i j))

; ex 2.12
(define (make-center-percent c p)
  (let ((v (* c p)))
    (make-interval (- c v) (+ c v))))

(define (center i)
  (average (lower-bound i) (upper-bound i)))

(define (percent i)
  (/ (- (upper-bound i) (lower-bound i)) (* 2 (center i))))

(make-center-percent 2 0.5)
(center i)
(percent i)