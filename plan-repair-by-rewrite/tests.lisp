(defpackage repair-by-rewrite-tests
  (:nicknames #:plan-repair-by-rewrite-tests)
  (:use #:plan-repair-rewrite #:iterate #:fiveam #:common-lisp))

(in-package #:repair-by-rewrite-tests)

(def-suite* plan-repair-rewrite-tests)

;;; Test for issue 2 -- incorrect rewrite of method subtasks
(test issue2
  (let* ((hddl-plan (hddl-utils:read-hddl-plan-file
                     (asdf:system-relative-pathname "plan-repair-rewrite" "data/iss2-plan.hddl")))
         (plan (getf
                (rest hddl-plan)
                :actions))
         (er
           (make-instance 'prr:execution-record
                          :domain (hddl-utils:read-hddl-file "home:projects;hideho;domains;rovers;domain.hddl")
                          :problem (hddl-utils:read-hddl-file "home:projects;hideho;domains;rovers;p01.hddl")
                          :plan plan
                          :unforeseen-pos NIL
                          :unforeseen-neg 'hddl::((visible waypoint2 waypoint3))
                          :executed-actions (subseq plan 0 6)))
         (repair-domain (repair-domain-and-problem er))
         (methods (hddl-utils:domain-methods repair-domain))
         (already-there (find 'hddl::already-there methods :key 'second)))
    (is-true (or (equalp (hddl-utils:hddlify-tree '(:method already-there :parameters (?rover - rover ?to - location) :task
                                                    (move-to ?rover ?to) :precondition (at ?rover ?to) :ordered-subtasks ()))
                         already-there)
                 (equalp (hddl-utils:hddlify-tree '(:method already-there :parameters (?rover - rover ?to - location) :task
                                                    (move-to ?rover ?to) :precondition (at ?rover ?to) :ordered-subtasks (and)))
                         already-there)))
    (is (equalp (hddl-utils:hddlify-tree
              ' (:method have-line-of-sight-for-soil :parameters
                    (?analysis-loc ?rover-loc - location ?rover - rover ?lander-loc - location ?l
                     - lander)
                  :task (transmit-soil ?analysis-loc ?rover-loc ?rover) :precondition
                  (and
                   (at ?rover ?rover-loc)
                   (at_lander ?l ?lander-loc)
                   (visible ?rover-loc ?lander-loc))
                  :ordered-subtasks
                  (and
                   (replay-communicate_soil_data-4-prime
                    ?rover
                    ?l
                    ?analysis-loc
                    ?rover-loc
                    ?lander-loc))))
             (find 'hddl::have-line-of-sight-for-soil methods :key 'second)))))
