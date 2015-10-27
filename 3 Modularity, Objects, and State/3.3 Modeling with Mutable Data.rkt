#lang planet neil/sicp

; ex 3.12
(define (append x y)
  (if (null? x)
      y
      (cons (car x) (append (cdr x) y))))

(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

; append! is a mutator
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))

(cdr x) ; '(b)
(define w (append! x y))
(cdr x) ; '(b c d)

; ex 3.13
(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define zz (make-cycle (list 'a 'b 'c))) ; circular list
(display zz) (newline)
; (last-pair zz) ; infinite recursion

; ex 3.14
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(display (mystery '(a b c d))) ; mystery inverses a list

; ex 3.15
(define z1 (cons x x))
(define z2 (cons '(a b c d) '(a b c d)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(set-to-wow! z1)
(set-to-wow! z2)
(display z1) (newline) (display z2) (newline)

; eq? tests the equality of pointers
(eq? (car z1) (cdr z1)) ; #t
(eq? (car z2) (cdr z2)) ; #f

; ex 3.16
; counts shared data multiple times
(define (bad-count-pairs x)
  (if (not (pair? x))
      0
      (+ (bad-count-pairs (car x))
         (bad-count-pairs (cdr x))
         1)))

; ! cons vs list
(bad-count-pairs '(a a a)) ; 3

(bad-count-pairs (let ((x '(a)))
                   (list x x))) ; 4

(bad-count-pairs (let ((x '(a)))
                   (let ((y (cons x x)))
                     (cons y y)))) ; 7

; (bad-count-pairs (make-cycle (list 'a 'b 'c))) ; never returns

; *ex 3.17
; memq tests if an item is in the list; if not, returns false; otherwise
; returns the sublist beginning with the first occurrence of item.
(define (memq item x)
  (cond ((null? x) #f)
        ((eq? (car x) item) x)
        (else (memq item (cdr x)))))

(define (count-pairs x)
  (let ((visited '())) ; visted pairs
    (define (iter x)
      (if (or (not (pair? x)) (memq x visited))
          0
          (begin
            (set! visited (cons x visited))
            (+ (iter (car x))
               (iter (cdr x))
               1))))
    (iter x)))

(count-pairs '(a a a)) ; 3

(count-pairs (let ((x '(a)))
               (list x x))) ; 3

(count-pairs (let ((x '(a)))
               (let ((y (cons x x)))
                 (cons y y)))) ; 3

(count-pairs (make-cycle (list 'a 'b 'c))) ; 3

; ** ex 3.18, ex 3.19 cycle detection
; this is wrong!
(define (bad-circular? x)
  (let ((visited '()))
    (define (visit x)
      (cond ((not (pair? x)) #f)
            ((memq x visited) #t) ; wrong
            (else (begin (set! visited (cons x visited))
                         (or (visit (car x))
                             (visit (cdr x)))))))
    (visit x)))

(bad-circular? (let ((x '(a))) (list x x))) ; #t

; using Tortoise and the Hare Algorithm, O(n) time, O(1) space
(define (circular? x)
  (define (run turtle rabbit)
    (cond ((eq? turtle rabbit) #t) ; rabbit catches up with turtle
          ((null? (cdr rabbit)) #f) ; rabbit reaches the end
          (else (run (cdr turtle) (cddr rabbit))))) ; rabbit moves faster than turtle
  (run x (cdr x)))

(circular? (let ((x '(a))) (list x x))) ; #f
(circular? (make-cycle (list 'a 'b 'c))) ; #t

