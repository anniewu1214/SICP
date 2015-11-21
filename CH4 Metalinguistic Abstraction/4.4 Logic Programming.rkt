#lang planet neil/sicp

;; a sample data base
(assert! (address (Bitdiddle Ben) (Slumerville (Ridge Road) 10)))
(assert! (job (Bitdiddle Ben) (computer wizard)))
(assert! (salary (Bitdiddle Ben) 60000))

(assert! (address (Hacker Alyssa P) (Cambridge (Mass Ave) 78)))
(assert! (job (Hacker Alyssa P) (computer programmer)))
(assert! (salary (Hacker Alyssa P) 40000))
(assert! (supervisor (Hacker Alyssa P) (Bitdiddle Ben)))

(assert! (address (Fect Cy D) (Cambridge (Ames Street) 3)))
(assert! (job (Fect Cy D) (computer programmer)))
(assert! (salary (Fect Cy D) 35000))
(assert! (supervisor (Fect Cy D) (Bitdiddle Ben)))

(assert! (address (Tweakit Lem E) (Boston (Bay State Road) 22)))
(assert! (job (Tweakit Lem E) (computer technician)))
(assert! (salary (Tweakit Lem E) 25000))
(assert! (supervisor (Tweakit Lem E) (Bitdiddle Ben)))

(assert! (address (Reasoner Louis) (Slumerville (Pine Tree Road) 80)))
(assert! (job (Reasoner Louis) (computer programmer trainee)))
(assert! (salary (Reasoner Louis) 30000))
(assert! (supervisor (Reasoner Louis) (Hacker Alyssa P)))

(assert! (supervisor (Bitdiddle Ben) (Warbucks Oliver)))
(assert! (address (Warbucks Oliver) (Swellesley (Top Heap Road))))
(assert! (job (Warbucks Oliver) (administration big wheel)))
(assert! (salary (Warbucks Oliver) 150000))

(assert! (address (Scrooge Eben) (Weston (Shady Lane) 10)))
(assert! (job (Scrooge Eben) (accounting chief accountant)))
(assert! (salary (Scrooge Eben) 75000))
(assert! (supervisor (Scrooge Eben) (Warbucks Oliver)))

(assert! (address (Cratchet Robert) (Allston (N Harvard Street) 16)))
(assert! (job (Cratchet Robert) (accounting scrivener)))
(assert! (salary (Cratchet Robert) 18000))
(assert! (supervisor (Cratchet Robert) (Scrooge Eben)))

(assert! (address (Aull DeWitt) (Slumerville (Onion Square) 5)))
(assert! (job (Aull DeWitt) (administration secretary)))
(assert! (salary (Aull DeWitt) 25000))
(assert! (supervisor (Aull DeWitt) (Warbucks Oliver)))

(assert! (can-do-job (computer wizard) (computer programmer)))
(assert! (can-do-job (computer wizard) (computer technician)))
(assert! (can-do-job (computer programmer) (computer programmer trainee)))
(assert! (can-do-job (administration secretary) (administration big wheel)))

;; simple queries (primitives)
(job ?x (computer programmer))
(address ?x ?y)
(supervisor ?x ?x)
(job ?x (computer ?type))
(job ?x (computer . ?type))

; ex 4.55
(supervisor ?person (Bitdiddle Ben))
(job ?person (accounting . ?type))
(address ?person (Slumerville . ?others))

;; compound queries (means of combination)
(and (job ?person (computer programmer))
     (address ?person ?where))

(or (supervisor ?x (Bitdiddle Ben))
    (supervisor ?x (Hacker Alyssa P)))

(and (supervisor ?x (Bitdiddle Ben))
     (not (job ?x (computer programmer))))

(and (salary ?person ?amount)
     (lisp-value > ?amount 30000))

; ex 4.56
(and (supervisor ?person (Bitdiddle Ben))
     (address ?person ?address))

(and (salary (Bitdiddle Ben) ?Ben-salary)
     (salary ?person ?salary)
     (lisp-value ?salary < ?Ben-salary))

(and (supervisor ?person ?supervisor)
     (not (job ?supervisor (computer . ?type)))
     (job ?supervisor ?job))

;; rules (means of abstraction)
(assert! (rule (lives-near ?person-1 ?person-2)
               (and (address ?person-1 (?town . ?rest-1))
                    (address ?person-2 (?town . ?rest-2))
                    (not (same ?person-1 ?person-2)))))

(assert! (rule (same ?x ?x)))

(assert! (rule (wheel ?person)
               (and (supervisor ?middle-manager ?person)
                    (supervisor ?x ?middle-manager))))

(assert! (and (job ?x (computer programmer))
              (lives-near ?x (Bitdiddle Ben))))

(assert!(rule (outranked-by ?staff-person ?boss)
              (or (supervisor ?staff-person ?boss)
                  (and (supervisor ?staff-person ?middle-manager)
                       (supervisor ?middle-manager ?boss)))))

; ex 4.57
(assert! (rule (replace ?person-1 ?person-2)
               (and (or (and (job ?person-1 ?job)
                             (job ?person-2 ?job))
                        (can-do-job ?person-1 ?person-2))
                    (not (same ?person-1 ?person-2)))))

(replace ?person (Fect Cy D))

(and (salary ?person-1 ?salary-1)
     (salary ?person-2 ?salary-2)
     (replace ?person-1 ?person-2)
     (lisp-value < ?person-1 ?person-2))

; ex 4.58
(assert! (rule (big-shot ?person)
               (and (job ?person (?division . ?rest))
                    (or (not (supervisor ?person ?boss))
                        (and (supervisor ?person ?boss)
                             (not (job ?boss (?division . ?rest-2))))))))

; ex 4.59
(assert! (meeting accounting (Monday 9am)))
(assert! (meeting administration (Monday 10am)))
(assert! (meeting computer (Wednesday 3pm)))
(assert! (meeting administration (Friday 1pm)))
(assert! (meeting whole-company (Wednesday 4pm)))

(assert! (meeting ?division (Friday ?time)))

(assert! (rule (meeting-time ?person ?day-and-time)
               (and (job ?person (?division . ?rest))
                    (or (meeting ?division ?day-and-time)
                        (meeting whole-company ?day-and-time)))))

(meeting-time (Hacker Alyssa P) (Wednesday ?time))

; ex 4.60
(assert! (rule (lives-near-better ?person-1 ?person-2)
               (and (list-value string>? ?person-1 ?person-2)
                    (address ?person-1 (?town . ?rest-1))
                    (address ?person-2 (?town . ?rest-2))
                    (not (same ?person-1 ?person-2)))))

;; logic as programs
(assert! (rule (append-to-form () ?y ?y)))
(assert! (rule (append-to-form (?u . ?v) ?y (?u . ?z))
               (append-to-form ?v ?y ?z)))

(append-to-form (a b) (c d) ?z)
(append-to-form (a b) ?y (a b c d))
(append-to-form ?x ?y (a b c d))

; ex 4.61
(assert! (rule (?x next-to ?y in (?x ?y . ?u))))
(assert! (rule (?x next-to ?y in (?v . ?z))
               (?x next-to ?y in ?z)))

(?x next-to ?y in (1 (2 3) 4))
; 1 next-to (2 3)
; (2 3) next-to 4

(?x next-to 1 in (2 1 3 1))
; 2 next-to 1
; 3 next-to 1

; ex 4.62
(assert! (rule (last-pair (?x) (?x))))
(assert! (rule (last-pair (?u . ?v) (?x))
               (last-pair (?v) (?x))))

(last-pair (3) ?x)
(last-pair (1 2 3) ?x)
(last-pair (2 ?x) (3))
; (last-pair ?x (3)) ; can't find dude

; ex 4.63
(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Irad Mehujael))
(assert! (son Mehujael Methushael))
(assert! (son Methushael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert! (rule (grandson ?S ?G)
               (and (son ?S ?f)
                    (son ?f ?G))))

(assert! (rule (son ?S ?M)
               (and (wife ?W ?M)
                    (sons ?S ?W))))

; ex 4.64
; inifite loop caused by (bad-outranked-by ?middle-manager ?boss)

; ex 4.65
; because (Warbucks Olivier) supervises 4 middle-managers

; ex 4.66
; the prb is queries like wheel may produce duplicate values, and
; the accumulated value will be biased in this case. Solution: filter
; duplicate frames in the stream before passing it to a mapping function

; ex 4.68
(assert! (rule (reverse () ())))

(assert! (rule (reverse (?x . ?rest) ?y)
               (and (reverse ?rest ?z)
                    (append-to-form ?z (?x) ?y))))

; ex 4.69
(assert! (rule (end-in-grandson (grandson))))
(assert!(rule (end-in-grandson (?x . ?rest))
              (end-in-grandson ?rest)))

(assert! (rule ((grandson) ?x ?y)
               (grandson ?x ?y)))

(assert! (rule ((great . ?rel) ?x ?y)
               (and (end-in-grandson ?rel)
                    (son ?x ?z)
                    (?rel ?z ?y))))