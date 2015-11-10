#lang planet neil/sicp

(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))

(define (prime? n)
  (define (smallest-divisor n test)
    (cond ((> (square test) n) n)
          ((= (remainder n test) 0) test)
          (else (smallest-divisor n (+ test 1)))))
  (if (= n 1)
      #f
      (= (smallest-divisor n 2) n)))

(define (require p) (if (not p) (amb)))

(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

(define (an-integer-starting-from n)
  (amb n (an-integer-starting-from (+ n 1))))

; ex 4.35
(define (an-integer-between low high)
  (require (>= high low))
  (amb low (an-integer-between (+ low 1) high)))

(define (square x) (* x x))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (square i) (square j)) (square k)))
        (list i j k)))))

; ex 4.36
; if we use an-integer-starting-from, it's highly likely that we'll never find a
; pythagorean triple. For example, if we start with i = 2, j = 2, we'll try all 
; possible k and get into infinite trying, because 2√2 is not an integer
(define (a-pythagorean-triple-starting-from low)
  (let ((high (an-integer-starting-from low)))
    (a-pythagorean-triple-between low high)))

; ex 4.37
; it's more efficient since it examines all (i,j) pairs instead of all (i, j, k) triples
(define (a-better-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (square high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (square i) (square j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))

; logic puzzles
(define (distinct? items)
  (cond ((null? items) #t)
        ((null? (cdr items)) #t)
        ((member (car items) (cdr items)) #f)
        (else (distinct? (cdr items)))))

; ex 4.39
; the order of restrictions doesn't matter, distint? is O(n^2) others
; are O(1), so we can improve efficency by testing distinct? lastly
(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (require (distinct? (list baker cooper fletcher miller smith)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith)))) ;; ((baker 3) (cooper 2) (fletcher 4) (miller 5) (smith 1))

; ex 4.40
(define (multiple-dwelling-better)
  (let ((fletcher (amb 2 4))
        (cooper (amb 2 4 5)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (let (miller (amb 3 4 5)) 
      (require (> miller cooper))
      (let ((smith (amb 1 2 4 5)))
        (require (not (= (abs (- smith fletcher)) 1)))
        (let ((baker (amb 1 2 3 4)))
          (require (distinct? (list baker cooper fletcher miller smith)))
          (list (list 'baker baker)
                (list 'cooper cooper)
                (list 'fletcher fletcher)
                (list 'miller miller)
                (list 'smith smith)))))))

; ex 4.41
(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
  (fold-right append '() (map proc seq)))

(define (permutations lists)
  (if (null? lists)
      '(())
      (flatmap (lambda (x)
                 (map (lambda (y) (cons x y))
                      (permutations (cdr lists))))
               (car lists))))

(define (multiple-dwelling-scheme)
  (let ((baker '(1 2 3 4))
        (cooper '(2 4))
        (fletcher '(2 4))
        (miller '(3 4 5))
        (smith '(1 2 4 5)))
    (filter restrictions (permutations (list baker cooper fletcher miller smith)))))
;; ((3 2 4 5 1))

(define (restrictions x)
  (apply
   (lambda (baker cooper fletcher miller smith)
     (and (> miller cooper)
          (not (= (abs (- smith fletcher)) 1))
          (not (= (abs (- fletcher cooper)) 1))
          (distinct? (list baker cooper fletcher miller smith))))
   x))

; ex 4.42
(define (liars-puzzle)
  (define (t-and-f a b) (not (equal? a b)))
  (let ((betty (amb 1 2 3 4 5))
        (ethel (amb 1 2 3 4 5))
        (joan (amb 1 2 3 4 5))
        (kitty (amb 1 2 3 4 5))
        (mary (amb 1 2 3 4 5)))
    (require (t-and-f (= kitty 2) (= betty 3)))
    (require (t-and-f (= ethel 1) (= joan 2)))
    (require (t-and-f (= joan 3) (= ethel 5)))
    (require (t-and-f (= kitty 2) (= mary 4)))
    (require (t-and-f (= mary 4) (= betty 1)))
    (require (distinct? (list betty ethel joan kitty mary)))
    (list (list 'betty betty)
          (list 'ethel ethel)
          (list 'joan joan)
          (list 'kitty kitty)
          (list 'mary mary))))

; ex 4.43
(define (father-daughter)
  (let ((Moore 'Mary)
        (Barnacle 'Melissa)
        (Hall (amb 'Gabrielle 'Lorna))
        (Downing (amb 'Gabrielle 'Lorna 'Rosalind)))
    (require (cond ((eq? Hall 'Gabrielle) (eq? Parker 'Rosalind))
                   ((eq? Downing 'Gabrielle) (eq? Parker 'Melissa))
                   (else #f)))
    (list (list Barnacle 'Barnacle)
          (list Moore 'Moore)
          (list Hall 'Hall)
          (list Downing 'Downing)
          (list Parker 'Parker))))

; parsing natural language
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))
(define prepositions '(prep for to in by with))

; S = NP + VP
(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))

; NP = ART + NN
; NP = NP + PP
(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

; VP = VERB
; VP = VP + PP
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

; PP = PREP + NP
(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unpassed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))