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

; *ex 2.26
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

; * ex 2.28
(define (fringe items)
  (define (iter items acc)
    (cond ((null? items) acc)
          ((not (pair? items)) (cons items acc))
          (else (iter (car items) (iter (cdr items) acc)))))
  (iter items nil))

(fringe (list (list 1 2) (list 3 4) 5))

; ex 2.29

; a. selectors and constructors
(define (make-mobile left right)
  (list left right))
(define left-branch car)
(define right-branch cadr)

(define (make-branch length structure)
  (list length structure))
(define branch-length car)
(define branch-structure cadr)

; b. total weight of mobile
(define structure-mobile? pair?)

(define (branch-weight branch)
  (let ((s (branch-structure branch)))
    (if (structure-mobile? s)
        (total-weight s)
        s)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define m0 (make-mobile
            (make-branch 1 (make-mobile (make-branch 1 2)
                                        (make-branch 2 4)))
            (make-branch 3 6)))

(total-weight m0)

; c. is the mobile balanced
(define (branch-balanced? branch)
  (let ((s (branch-structure branch)))
    (if (structure-mobile? s)
        (balanced? s)
        #t)))

(define (branch-torque branch)
  (* (branch-weight branch)
     (branch-length branch)))

(define (balanced? mobile)
  (let ((r (right-branch mobile)) (l (left-branch mobile)))
    (and (= (branch-torque r)
            (branch-torque l))
         (branch-balanced? l)
         (branch-balanced? r))))

(balanced? (make-mobile (make-branch 2 1) 
                        (make-branch 1 2))) 

; d. change representation
(define (make-mobile-2 left right)
  (cons left right))
(define (make-branch-2 length structure)
  (cons length structure))

(define right-branch-2 cdr)
(define branch-structure-2 cdr)

; ex 2.30
(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(display
 (square-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))))

(define (square-tree-2 tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-2 sub-tree)
             (square sub-tree)))
       tree))

(newline)
(display
 (square-tree-2 (list 1 (list 2 (list 3 4) 5) (list 6 7))))

; ex 2.31
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree-3 tree) (tree-map square tree))

(newline)
(display
 (square-tree-3 (list 1 (list 2 (list 3 4) 5) (list 6 7))))

; *ex 2.32
; the set of all subsets of s contains:
; 1. all subsets excluding the first element of s
; 2. all subsets in (1.) with the first element inserted
(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map
                 (lambda (x) (cons (car s) x))
                 rest)))))


; e.g. sequence operation
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence) (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

; enumerate intergers in a range
(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (inc low) high))))

(enumerate-interval 2 7)

; enumerate leaves of a tree (fringe)
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(enumerate-tree (list 1 (list 2 (list 3 4)) 5))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map square (filter odd? (enumerate-tree tree)))))

(define (fib n)
  (define (fib_ac n a b)
    (if (= n 0)
        b
        (fib_ac (- n 1) (+ a b) a)))
  (fib_ac n 1 0))

(define (even-fibs n)
  (accumulate
   cons
   nil
   (filter even? (map fib (enumerate-interval 0 n)))))

; ex 2.33
(define (i-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(define (i-append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (i-length sequence)
  (accumulate (lambda (x y) (inc y)) 0 sequence))

(newline)
(display (i-map square (list 1 2 3)))
(newline)
(display (i-append (list 1 2) (list 3 4)))
(newline)
(display (i-length (list (list 1 2) 3 4)))
(newline)

; ex 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))

; ex 2.35
(define (count-leaves t)
  (accumulate +
              0
              (map (lambda (x)
                     (if (pair? x)
                         (count-leaves x)
                         1))
                   t)))

(count-leaves (list (list 1 2) 3 4 (list 5 6) 7))

; ex 2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9)))

; ex 2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row-seq) (dot-product row-seq v)) m))

(define (transpose mat)
  (accumulate-n cons nil mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (v) (matrix-*-vector cols v)) m)))

(newline)
(define v (list 1 1 1))
(define m (list (list 1 1 1) (list 1 1 1) (list 1 1 1)))
(dot-product v v)
(matrix-*-vector m v)
(newline)
(display (matrix-*-matrix m m))

; ex 2.38
(define fold-right accumulate)

(define (fold-left op init sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter init sequence))

(newline)
(fold-right / 1 (list 1 2 3)) ; 3/2
(fold-left / 1 (list 1 2 3)) ; 1/6
(display (fold-right list nil (list 1 2 3))) ; (1 (2 (3 ())))
(newline)
(display (fold-left list nil (list 1 2 3))) ; (((() 1) 2) 3)

; fold-left and fold-right will produce the same value if
; (op a b) equals (op b a)

; ex 2.39
(define (reverse-left-fold sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))
(define (reverse-right-fold sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(newline)
(reverse-left-fold (list 1 2 3))
(reverse-right-fold (list 1 2 3))

; e.g. nested mappings
(define (prime? n)
  (define (smallest-divisor n test)
    (cond ((> (square test) n) n)
          ((= (remainder n test) 0) test)
          (else (smallest-divisor n (+ test 1)))))
  
  (if (= n 1)
      #f
      (= (smallest-divisor n 2) n)))

(define (flatmap proc seq)
  (fold-right append nil (map proc seq)))

; find all (i,j) where 1 <= j < i <= n and (i + j) is prime
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (dec i))))
                (enumerate-interval 1 n)))))

(newline)
(display (prime-sum-pairs 6))

; generate set permutation
; for each item x in S, recursively generate permutation of S-x, and
; ajoint x to the front of each one.
(define (permutations s)
  (define (remove item seq)
    (filter (lambda (x) (not (= x item))) seq))
  
  (if (null? s)
      (list nil)
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))
(newline)
(display (permutations (list 1 2 3)))

; ex 2.40
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (dec i))))
   (enumerate-interval 1 n)))

(newline)
(display (unique-pairs 3))

(define (prime-sum-pairs-2 n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))

(newline)
(display (prime-sum-pairs-2 6))

; ex 2.41
(define (unique-triples n)
  (flatmap
   (lambda (i)
     (flatmap (lambda (j)
                (map (lambda (k) (list i j k))
                     (enumerate-interval 1 (dec j))))
              (enumerate-interval 1 (dec i))))
   (enumerate-interval 1 n)))

(newline)
(display (unique-triples 5))

; triples (i, j, k) i < j < k <= n, (+ i j k) = s
(define (triple-sum-to-s n s)
  (filter (lambda (triple) (= s (fold-right + 0 triple)))
          (unique-triples n)))

(newline)
(display (triple-sum-to-s 7 6))

; *ex 2.42, 8-queens puzzle
(define empty-board nil)

(define (adjoin-positions new-row k rest-of-queens)
  (cons (list k new-row) rest-of-queens))

(define (safe? k positions)
  (define (safe-row?)
    (null? (filter (lambda (pos)
                     (= (cadr pos) (cadar positions)))
                   (cdr positions))))
  (define (safe-diag?)
    (null? (filter (lambda (pos)
                     (= (abs (- (caar positions) (car pos))) (abs (- (cadar positions) (cadr pos)))))
                   (cdr positions))))
  (and (safe-row?) (safe-diag?)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-positions new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (dec k))))))
  (queen-cols board-size))