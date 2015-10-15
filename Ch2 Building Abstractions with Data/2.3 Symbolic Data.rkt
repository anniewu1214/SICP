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

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define addend cadr)

; ex 2.57
(define (augend exp)
  (if (> (length exp) 3)
      (cons '+ (cddr exp))
      (caddr exp)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define multiplier cadr)

; ex 2.57
(define (multiplicand exp)
  (if (> (length exp) 3)
      (cons '* (cddr exp))
      (caddr exp)))

; ex 2.56
(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define base cadr)
(define exponent caddr)

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((=number? base 1) 1)
        (else (list '** base exponent))))

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
        ((exponentiation? exp)
         (make-product (exponent exp)
                       (make-product (make-exponentiation (base exp)
                                                          (make-sum (exponent exp) -1))
                                     (deriv (base exp) var))))
        (else
         (error "unknow expression type -- DERIV" exp))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(display (deriv '(* (* x y) (+ x 3)) 'x))
(newline)
(display (deriv '(** u n) 'u))
(newline)
(display (deriv '(* x y (+ x 3)) 'x))

; ex 2.58
(define (sum? exp)
  (and (pair? exp) (eq? '+ (cadr exp))))

(define addend car)
(define augend caddr)

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (product? x)
  (and (pair? x) (eq? '* (cadr x))))

(define multiplier car)
(define multiplicand caddr)

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(newline)
(display (deriv '(x + (3 * (x + (y + 2)))) 'x))

; ex 2.59, sets as unordered lists
(define s1 '(1 2 3 4))
(define s2 '(4 5 6 1))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? x (car set)) #t)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 1 s1)
(element-of-set? 100 s1)

(define (ajoint-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(ajoint-set 1 s1)
(ajoint-set 100 s1)

(define (intersection-set s1 s2)
  (cond ((or (null? s1) (null? s2)) nil)
        ((element-of-set? (car s1) s2)
         (cons (car s1) (intersection-set (cdr s1) s2)))
        (else (intersection-set (cdr s1) s2))))

(intersection-set s1 s2)

(define (union-set s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        ((element-of-set? (car s1) s2)
         (union-set (cdr s1) s2))
        (else (cons (car s1) (union-set (cdr s1) s2)))))

(union-set s1 s2)

; ex 2.60
(define (adjoin-set x set) (cons x set))
(define (union-set s1 s2) (append s1 s2))

; ex 2.61, sets as ordered lists
(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set-list s1 s2)
  (if (or (null? s1) (null? s2))
      nil
      (let ((x1 (car s1)) (x2 (car s2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr s1)
                                       (cdr s2))))
              ((< x1 x2)
               (intersection-set (cdr s1) s2))
              ((> x1 x2)
               (intersection-set s1 (cdr s2)))))))

(define (adjoin-set-list x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

(define (union-set-list s1 s2)
  (cond ((null? s1) s2)
        ((null? s2) s1)
        (else
         (let ((x1 (car s1)) (x2 (car s2)))
           (cond ((= x1 x2) (cons x1 (union-set (cdr s1) (cdr s2))))
                 ((< x1 x2) (cons x1 (union-set (cdr s1) s2)))
                 ((> x1 x2) (cons x2 (union-set s1 (cdr s2)))))))))

; ex 2.63, sets as binary trees
(define entry car)
(define left-branch cadr)
(define right-branch caddr)
(define make-tree list)

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set)) (element-of-set? x (left-branch set)))
        ((> x (entry set)) (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x nil nil))
        ((= x (entry set)) set)
        ((< x (entry set)) (make-tree (entry set)
                                      (adjoin-set x (left-branch set))
                                      (right-branch set)))
        ((> x (entry set)) (make-tree (entry set)
                                      (left-branch set)
                                      (adjoin-st x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      nil
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result)
    (if (null? tree)
        result
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result)))))
  (copy-to-list tree nil))

; they produce the same result, both implement in-order traversal
; time complexity of tree->list-1 is O(n*log n), and O(n) for tree->list-2
; the major difference resides in the append vs. cons in the divide-conquer process
(define tree '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(tree->list-1 tree) ; (1 3 5 7 9 11)
(tree->list-2 tree) ; (1 3 5 7 9 11)

; ex 2.64
; a. partial-tree takes the median item of the list and creates a binary tree
; rooted at this item, whose left tree and right tree are results of calling
; partial-tree recursively on the left and right items. 
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

; b. T(n) = 2T(n/2) + O(1) => T(n) = O(n) => linear time complexity
(define (partial-tree elts n)
  (if (= n 0)
      (cons nil elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts) right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree
                                 remaining-elts)))))))))

(list->tree '(1 3 5 7 9)) ; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

; ex 2.65
(define (union-set s1 s2)
  (list->tree (union-set-list (tree->list2 s1)
                              (tree->list2 s2))))

(define (intersection-set s1 s2)
  (list->tree (intersection-set-list (tree-list2 s1)
                                     (tree-list2 s2))))

; ex 2.66
(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) #f)
        ((= given-key (key (entry set-of-records)))
         (entry set-of-records))
        ((< given-key (key (entry set-of-records)))
         (lookup given-key (left-branch set-of-records)))
        (else (lookup given-key (right-branch set-of-records)))))


; Huffman encoding
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define symbol-leaf cadr)
(define weight-leaf caddr)

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define left-branch car)
(define right-branch cadr)

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE_BRANCH" bit))))

; Huffman decoding
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        nil
        (let ((next-branch (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

; ex 2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-msg '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(display (decode sample-msg sample-tree)) ; ADABBCA

; ex 2.68
(define (encode msg tree)
  (if (null? msg)
      nil
      (append (encode-symbol (car msg) tree)
              (encode (cdr msg) tree))))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((equal? (car set) x) #t)
        (else (element-of-set? x (cdr set)))))

(define (encode-symbol symbol tree)
  (define (branch-contains-symbol branch)
    (if (leaf? branch)
        (equal? symbol (symbol-leaf branch))
        (element-of-set? symbol (symbols branch))))
  
  (let ((r (right-branch tree))
        (l (left-branch tree)))
    (cond ((branch-contains-symbol r)
           (if (leaf? r) (list 1) (cons 1 (encode-symbol symbol r))))
          ((branch-contains-symbol l)
           (if (leaf? l) (list 0) (cons 0 (encode-symbol symbol l))))
          (else (error "symbol not in the tree -- ENCODE-SYMBOL" symbol)))))

(display (encode '(A D A B B C A) sample-tree)) ; (0 1 1 0 0 1 0 1 0 1 1 1 0)

; ex 2.69
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

; pairs is of format ((A 4) (B 2) (C 1) (D 1))
(define (make-leaf-set pairs)
  (if (null? pairs)
      nil
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) (cadr pair))
                    (make-leaf-set (cdr pairs))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (successive-merge (adjoin-set (make-code-tree (car leaf-set)
                                                    (cadr leaf-set))
                                    (cddr leaf-set)))))

(define H-tree (generate-huffman-tree '((A 4) (C 1) (D 1) (B 2))))
(display (encode '(A D A B B C A) H-tree)) ; (0 1 1 0 0 1 0 1 0 1 1 1 0)

; ex 2.70
(define lyrics-H-tree
  (generate-huffman-tree '((a 2) (boom 1) (get 2) (job 2)
                                 (na 16) (sha 3) (yip 9) (wah 1))))

(define lyrics '(get a job sha na na na na na na na na get a job
                     sha na na na na na na na na wah yip yip yip
                     yip yip yip yip yip yip sha boom))

(define encoded-lyrics
  (encode lyrics lyrics-H-tree))
(length encoded-lyrics)
(* (length lyrics) 3) ; 3 bits per symbol for fixed-length encoding

; ex 2.71
; 1 bit for the most frequent symbol, (n - 1) bit for the least frequent symbol