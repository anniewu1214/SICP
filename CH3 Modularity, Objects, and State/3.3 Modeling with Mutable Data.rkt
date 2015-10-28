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

; ex 3.24, ex 3.25, general N dimentional table
(define (make-general-table same-key?)
  (let ((local-table (list '*table*)))
    ; returns the record that has the given key as its car
    (define (assoc key records)
      (cond ((null? records) #f)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    ; returns the value of a key, false if not in table
    (define (lookup key-list)
      (define (iter keys table)
        (let ((subtable (assoc (car keys) (cdr table))))
          (if subtable
              (if (null? (cdr keys))
                  (cdr subtable)
                  (iter (cdr keys) subtable))
              #f)))
      (iter key-list local-table))
    
    (define (insert! key-list value)
      (define (make-entry keys)
        (if (null? (cdr keys))
            (cons (car keys) value)
            (list (car keys) (make-entry (cdr keys)))))
      
      (define (iter keys table)
        (let ((subtable (assoc (car keys) (cdr table))))
          (if subtable
              (if (null? (cdr keys))
                  (set-cdr! subtable value)
                  (iter (cdr keys) subtable))
              (set-cdr! table
                        (cons (make-entry keys)
                              (cdr table))))))
      (iter key-list local-table))
    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define g-table (make-general-table equal?))
(define get (g-table 'lookup-proc))
(define put (g-table 'insert-proc!))

(put '(letter a) 97)
(put '(letter b) 98)
(put '(greek majiscule alpha) 923)
(put '(greek miniscule lambda) 955)

(get '(letter b))
(get '(greek majiscule alpha))

; ex 3.26, binary search tree
(define (entry tree) (car tree))
(define (left-branch  tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right) (list entry left right))

(define (lookup-tree key records)
  (if (null? records)
      #f
      (let ((record (entry records)))
        (cond ((= key (car record)) record)
              ((< key (car record)) (lookup-tree key (left-branch records)))
              (else (lookup-tree key (right-branch records)))))))

(define (insert-tree record records)
  (cond ((null? records) (make-tree record nil nil))
        ((= (car record) (car (entry records))) records)
        ((< (car record) (car (entry records)))
         (make-tree (entry records)
                    (insert-tree record (left-branch records))
                    (right-branch records)))
        (else
         (make-tree (entry records)
                    (left-branch records)
                    (insert-tree record (right-branch records))))))

(define (make-table-BST)
  (let ((local-table '()))
    
    (define (lookup key)
      (define (iter key records)
        (cond ((null? records) #f)
              ((= key (car (entry records))) (entry records))
              ((< key (car (entry records))) (iter key (left-branch records)))
              (else (iter key (right-branch records)))))
      (iter key local-table))
    
    (define (insert! key value)
      (let ((record (lookup key)))
        (if record
            (set-cdr! record value)
            (set! local-table (insert-tree (cons key value) local-table)))))
    
    (define (dispatch m)
      (cond ((eq? m 'get-proc) lookup)
            ((eq? m 'put-proc) insert!)
            ((eq? m 'print-proc) local-table)
            (else (error "Undefined operation -- TABLE" m))))
    dispatch))

(define table-BST (make-table-BST))
(define get (table-BST 'get-proc))
(define put (table-BST 'put-proc))

(put 97 'a)
(put 98 'b)
(put 99 'c)
(put 100 'd)

(display (table-BST 'print-proc))
(get 100)

; ex 3.27, memoization
(define (memorize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((pre-result (lookup x table)))
        (or pre-result
            (let ((result (f x)))
              (insert! x result table)
              result))))))

(define memo-fib
  (memorize (lambda (n)
              (cond ((= n 0) 0)
                    ((= n 1) 1)
                    (else (+ (memo-fib (- n 1))
                             (memo-fib (- n 2))))))))

(memo-fib 4)

; * a simulator for digital circuits
(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire)) (c1 (make-wire)) (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

; associate with the input wire a procedure that will be run whenever
; the signal on the input changes value
(define (inverter input output)
  
  (define (logical-not s)
    (cond ((= s 0) 1)
          ((= s 1) 0)
          (else (error "Invalid signal" s))))
  
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  
  (add-action! input invert-input)
  'ok)

; the action procedure will be run if either of the inputs to the gate changes
(define (and-gate a1 a2 output)
  (define (logical-and x y)
    (if (or (and (= x 1) (= y 1))
            (and (= x 0) (= y 0)))
        1
        0))
  
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

; ex 3.28
(define (or-gate a1 a2 output)
  (define (logical-or x y)
    (if (or (= x 1) (= y 1))
        1
        0))
  
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

; ex 3.29
; (A or B) <=> (not ((not A) and (not B)))
; and-gate-delay = or-gate-delay + 2 * inverter-delay
(define (or-gate-2 a1 a2 output)
  (let ((c1 (make-wire)) (c2 (make-wire)) (c3 (make-wire)))
    (inverter a1 c1)
    (inverter a2 c2)
    (and-gate c1 c2 c3)
    (inverter c3 output)))

; ex 3.30, ripple-carry adder
(define (ripple-carry-adder A B S C)
  (let ((c-in (make-wire)))
    (if (null? (cdr A))
        (set-signal! c-in 0)
        (ripple-carry-adder (cdr A) (cdr B) (cdr S) c-in))
    (full-adder (car A) (car B) c-in (car S) C)))

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    
    ; ex 3.31
    ; To trigger actions, otherwise the agenda tabel will be empty and no actions will be taken.
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

; asserts that the designated proc should be run whenever the signal changes
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

; execute proc after a delay
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

; agenda
(define (make-time-segment time queue) (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time) (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments) (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda) (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments)) action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr! segments
                        (cons (make-new-time-segment time action)
                              (cdr segments)))
              (add-to-segments! rest)))))
  
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty: FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

; execute each proc on the agenda in sequence
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

; add a probe on a wire
(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline) (display name) (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

; define 4 wires, placing probes on two of them
(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))

(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)
(set-signal! input-1 1)

(propagate)                        