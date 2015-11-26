#lang planet neil/sicp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; represent memory as vectors ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; primitive memory operations
(define (vector-ref <vector> <n>)
  "return the nth element of the vector")

(define (vector-set! <vector> <n> <value>)
  "set the nth element of the vector to <value>")


; represent Lisp data: typed pointers
;
; - pair: divide memory into two vectors: the-cars, the-cdrs;
;   A pointer to a pair is an index into the two vectors.
; - number: a pointer a a number consists of a type indicating
;   numerical data with the actual representation of the number
; - symbol: interning symbols


; implementing primitive list operations
; * assume that operations on pointers use only the index portion of the typed pointer

;; car, cdr
(assign <reg1> (op car) (reg <reg2>)) 
(assign <reg1> (op cdr) (reg <reg2>)) ; =>
(assign <reg1> (op vector-ref) (reg the-cars) (reg <reg2>))
(assign <reg1> (op vector-ref) (reg the-cdrs) (reg <reg2>))

;; set-car!, set-cdr!
(perform (op set-car!) (reg <reg1>) (reg <reg2>))
(perform (op set-cdr!) (reg <reg1>) (reg <reg2>)) ; =>
(perform (op vector-set!) (reg the-cars) (reg <reg1>) (reg <reg2>))
(perform (op vector-set!) (reg the-cdrs) (reg <reg1>) (reg <reg2>))

;; cons
(assign <reg1> (op cons) (reg <reg2>) (reg <reg3>)) ; =>
(perform (op vector-set!) (reg the-cars) (reg free) (reg <reg2>))
(perform (op vector-set!) (reg the-cdrs) (reg free) (reg <reg3>))
(assign <reg1> (reg free))
(assign free (op +) (reg free) (const 1))

;; eq?: tests the equality of all fields in the registers
;;      equality of data objects <=> identicality of their pointers
;; pair?, null?, symbol?, number? : check the type field


; implementing stacks
(save <reg>) ; =>
(assign the-stack (op cons) (reg <reg>) (reg the-stack))

(restore <reg>) ; =>
(assign <reg> (op car) (reg the-stack))
(assign the-stack (op cdr) (reg the-stack))

(perform (op initialize-stack)) ; =>
(

 
