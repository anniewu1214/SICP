#lang planet neil/sicp

;; a sample data base
(address (Bitdiddle Ben) (Slumerville (Ridge Road) 10))
(job (Bitdiddle Ben) (computer wizard))
(salary (Bitdiddle Ben) 60000)

(address (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
(job (Hacker Alyssa P) (computer programmer))
(salary (Hacker Alyssa P) 40000)
(supervisor (Hacker Alyssa P) (Bitdiddle Ben))

(address (Fect Cy D) (Cambridge (Ames Street) 3))
(job (Fect Cy D) (computer programmer))
(salary (Fect Cy D) 35000)
(supervisor (Fect Cy D) (Bitdiddle Ben))

(address (Tweakit Lem E) (Boston (Bay State Road) 22))
(job (Tweakit Lem E) (computer technician))
(salary (Tweakit Lem E) 25000)
(supervisor (Tweakit Lem E) (Bitdiddle Ben))

(address (Reasoner Louis) (Slumerville (Pine Tree Road) 80))
(job (Reasoner Louis) (computer programmer trainee))
(salary (Reasoner Louis) 30000)
(supervisor (Reasoner Louis) (Hacker Alyssa P))

(supervisor (Bitdiddle Ben) (Warbucks Oliver))
(address (Warbucks Oliver) (Swellesley (Top Heap Road)))
(job (Warbucks Oliver) (administration big wheel))
(salary (Warbucks Oliver) 150000)

(address (Scrooge Eben) (Weston (Shady Lane) 10))
(job (Scrooge Eben) (accounting chief accountant))
(salary (Scrooge Eben) 75000)
(supervisor (Scrooge Eben) (Warbucks Oliver))

(address (Cratchet Robert) (Allston (N Harvard Street) 16))
(job (Cratchet Robert) (accounting scrivener))
(salary (Cratchet Robert) 18000)
(supervisor (Cratchet Robert) (Scrooge Eben))

(address (Aull DeWitt) (Slumerville (Onion Square) 5))
(job (Aull DeWitt) (administration secretary))
(salary (Aull DeWitt) 25000)
(supervisor (Aull DeWitt) (Warbucks Oliver))

(can-do-job (computer wizard) (computer programmer))
(can-do-job (computer wizard) (computer technician))
(can-do-job (computer programmer) (computer programmer trainee))
(can-do-job (administration secretary) (administration big wheel))

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
(rule (lives-near ?person-1 ?person-2)
      (and (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))

(rule (same ?x ?x))

(rule (wheel ?person)
      (and (supervisor ?middle-manager ?person)
           (supervisor ?x ?middle-manager)))

(and (job ?x (computer programmer))
     (lives-near ?x (Bitdiddle Ben)))

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (supervisor ?staff-person ?middle-manager)
               (supervisor ?middle-manager ?boss))))

; ex 4.57
(rule (replace ?person-1 ?person-2)
      (and (or (and (job ?person-1 ?job)
                    (job ?person-2 ?job))
               (can-do-job ?person-1 ?person-2))
           (not (same ?person-1 ?person-2))))

(replace ?person (Fect Cy D))

(and (salary ?person-1 ?salary-1)
     (salary ?person-2 ?salary-2)
     (replace ?person-1 ?person-2)
     (lisp-value < ?person-1 ?person-2))

; ex 4.58
(rule (big-shot ?person)
      (and (job ?person (?division . ?rest))
           (or (not (supervisor ?person ?boss))
               (and (supervisor ?person ?boss)
                    (not (job ?boss (?division . ?rest-2)))))))

; ex 4.59
(meeting accounting (Monday 9am))
(meeting administration (Monday 10am))
(meeting computer (Wednesday 3pm))
(meeting administration (Friday 1pm))
(meeting whole-company (Wednesday 4pm))

(meeting ?division (Friday ?time))

(rule (meeting-time ?person ?day-and-time)
      (and (job ?person (?division . ?rest))
           (or (meeting ?division ?day-and-time)
               (meeting whole-company ?day-and-time))))

(meeting-time (Hacker Alyssa P) (Wednesday ?time))

; ex 4.60
(rule (lives-near-better ?person-1 ?person-2)
      (and (list-value string>? ?person-1 ?person-2)
           (address ?person-1 (?town . ?rest-1))
           (address ?person-2 (?town . ?rest-2))
           (not (same ?person-1 ?person-2))))