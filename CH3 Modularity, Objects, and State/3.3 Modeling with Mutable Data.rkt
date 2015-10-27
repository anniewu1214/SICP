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

; ex 3.20, (cdr (cons a b)) => b; (car (cons a b)) => a
(define (i-cons x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) (lambda (v) (set! x v)))
          ((eq? m 'set-cdr!) (lambda (v) (set! y v)))
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (i-car z) (z 'car))
(define (i-cdr z) (z 'cdr))
(define (set-i-car! z new-value) (z 'set-car! new-value))
(define (set-i-cdr! z new-value) (z 'set-cdr! new-value))

; queue
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           (print-queue queue))
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           (print-queue queue)))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         (print-queue queue))))

; ex 3.21, pretty print queue
(define (print-queue queue)
  (display (front-ptr queue))
  (newline))

(define q (make-queue))
(insert-queue! q 'a)
(insert-queue! q 'b)
(delete-queue! q)
(delete-queue! q)

; ex 3.22, build queue as a procedure with local state
(define (make-queue-local)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?) (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT! called with empty queue")
          (car front-ptr)))
    (define (insert-queue! item)
      (let ((new-pair (list item)))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)))
        front-ptr))
    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with empty queue"))
            (else
             (set! front-ptr (cdr front-ptr))
             front-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue) empty-queue?)
            ((eq? m 'front-queue) front-queue)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            (else (error "Undefined operation -- QUEUE" m))))
    dispatch))

; ex 3.23, deque, using doubly linked list
(define (make-deque) (cons nil nil))
(define (empty-deque? deque) (null? (front-ptr deque)))

(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (caar (front-ptr deque))))
  
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (caar (rear-ptr deque))))
  
(define (front-insert-deque! deque item)
  (let ((new-pair (cons (cons item nil) nil)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           (print-deque deque))
          (else
           (set-cdr! new-pair (front-ptr deque))
           (set-cdr! (car (front-ptr deque)) new-pair)
           (set-front-ptr! deque new-pair)
           (print-deque deque)))))
           
(define (rear-insert-deque! deque item)
  (let ((new-pair (cons (cons item nil) nil)))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-pair)
           (set-rear-ptr! deque new-pair)
           (print-deque deque))
          (else
           (set-cdr! (rear-ptr deque) new-pair)
           (set-cdr! (car new-pair) (rear-ptr deque))
           (set-rear-ptr! deque new-pair)
           (print-deque deque)))))

(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "DELETE! called with an empty deque" deque))
        (else
         (set-front-ptr! deque (cdr (front-ptr deque)))
         (print-deque deque))))

(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error ("DELETE! called with an empty deque" deque)))
        (else
         (set-rear-ptr! deque (cdar (rear-ptr deque)))
         (set-cdr! (rear-ptr deque) nil)
         (print-deque deque))))

(define (print-deque deque)
  (define (iter q)
    (if (or (null? q) (empty-deque? q))
        (display "")
        (begin (display (caar q)) (iter (cdr q)))))
  (iter (front-ptr deque)) (newline))

(define dq (make-deque))
(front-insert-deque! dq 'b) ; b
(front-insert-deque! dq 'a) ; ab
(rear-insert-deque! dq 'c) ; abc
(rear-delete-deque! dq) ; ab
(front-delete-deque! dq) ; b
(front-delete-deque! dq) ; '()

; 1D table
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        #f)))

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table))))))

(define (make-table) (list '*table*))

; 2D table
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              #f))
        #f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table))))))

(define (make-table-object)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))