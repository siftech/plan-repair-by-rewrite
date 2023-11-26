(defpackage repair-by-rewrite-tests
  (:nicknames #:plan-repair-by-rewrite-tests)
  (:import-from #:hddl-utils #:domain-actions
                #:action-name #:task-name #:domain-tasks #:method-subtasks #:method-name
                #:domain-methods)
  (:use #:plan-repair-rewrite #:iterate #:fiveam #:common-lisp))

(in-package #:repair-by-rewrite-tests)

(def-suite* plan-repair-rewrite-tests)

(defparameter +data-pathname+ (asdf:system-relative-pathname "plan-repair-rewrite" "data/"))

(defparameter *shop-rover-plan*
  (hddl-utils:hddlify-tree
   '((NAVIGATE ROVER0 WAYPOINT3 WAYPOINT1)
     (NAVIGATE ROVER0 WAYPOINT1 WAYPOINT2)
     (SAMPLE_SOIL ROVER0 ROVER0STORE WAYPOINT2)
     (COMMUNICATE_SOIL_DATA ROVER0 GENERAL WAYPOINT2 WAYPOINT2 WAYPOINT0)
     (NAVIGATE ROVER0 WAYPOINT2 WAYPOINT1)
     (NAVIGATE ROVER0 WAYPOINT1 WAYPOINT3)
     (DROP ROVER0 ROVER0STORE)
     (SAMPLE_ROCK ROVER0 ROVER0STORE WAYPOINT3)
     (COMMUNICATE_ROCK_DATA ROVER0 GENERAL WAYPOINT3 WAYPOINT3 WAYPOINT0)
     (NAVIGATE ROVER0 WAYPOINT3 WAYPOINT0)
     (CALIBRATE ROVER0 CAMERA0 OBJECTIVE1 WAYPOINT0)
     (TAKE_IMAGE ROVER0 WAYPOINT0 OBJECTIVE1 CAMERA0 HIGH_RES)
     (NAVIGATE ROVER0 WAYPOINT0 WAYPOINT3)
     (NAVIGATE ROVER0 WAYPOINT3 WAYPOINT1)
     (COMMUNICATE_IMAGE_DATA ROVER0 GENERAL OBJECTIVE1 HIGH_RES WAYPOINT1 WAYPOINT0))))

;;; Test for issue 1
(test issue1
  (let* ((orig-domain (hddl-utils:read-hddl-file (merge-pathnames "rover-domain.hddl"
                                                                  +data-pathname+)))
         (orig-problem (hddl-utils:read-hddl-file (merge-pathnames "rover-p01.hddl"
                                                                   +data-pathname+)))
         (orig-plan (let ((copy (copy-tree *shop-rover-plan*)))
                      (iter (for i from 1)
                        (as step in copy)
                        (collecting (cons i step)))))
         (erecord
           (make-instance 'execution-record
                          :plan orig-plan
                          :problem orig-problem
                          :domain orig-domain
                          :executed-actions (subseq orig-plan 0 6)
                          :unforeseen-neg (hddl-utils:hddlify-tree '((VISIBLE WAYPOINT3 WAYPOINT0)))
                          :unforeseen-pos nil))
         (new-domain
           (repair-domain-and-problem erecord)))
    (is-true (and (find 'hddl::navigate-1-prime (domain-actions new-domain) :key #'action-name)
                  (find 'hddl::navigate-2-prime (domain-actions new-domain) :key #'action-name)
                  (find 'hddl::navigate-5-prime (domain-actions new-domain) :key #'action-name)
                  (find 'hddl::navigate-6-prime (domain-actions new-domain) :key #'action-name)))
    (is-true (find 'hddl::play-or-replay-navigate (domain-tasks new-domain) :key #'task-name))
    (flet ((find-subtask (method-name task-name)
             (let* ((method (or (find method-name (domain-methods new-domain) :key #'method-name)
                                (error "Couldn't find method named ~s in domain" method-name)))
                    (subtasks (method-subtasks method)))
               (setf subtasks (cond ((eq (first subtasks) 'and)
                                     (rest subtasks))
                                    ((symbolp (first subtasks))
                                     (list subtasks))
                                    ((null subtasks) nil)
                                    (t (error "Unexpected set of subtasks: ~s" subtasks))))
               (is-true (find task-name subtasks :key #'first)))))
      (find-subtask 'hddl::go-there 'hddl::play-or-replay-navigate)
      (find-subtask 'hddl::have-line-of-sight-for-soil 'hddl::play-or-replay-communicate_soil_data)
      (find-subtask 'hddl::achieve-communicated-soil-data 'hddl::play-or-replay-sample_soil))))

;;; Test for issue 2 -- incorrect rewrite of method subtasks
(test issue2
  (let* ((hddl-plan (hddl-utils:read-hddl-plan-file
                     (asdf:system-relative-pathname "plan-repair-rewrite" "data/iss2-plan.hddl")))
         (plan (getf
                (rest hddl-plan)
                :actions))
         (domain (hddl-utils:read-hddl-file (merge-pathnames "rover-domain.hddl"
                                                                  +data-pathname+)))
         (problem (hddl-utils:read-hddl-file (merge-pathnames "rover-p01.hddl"
                                                                   +data-pathname+)))
         (er
           (make-instance 'prr:execution-record
                          :domain domain
                          :problem problem
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
                      (play-or-replay-communicate_soil_data
                       ?rover
                       ?l
                       ?analysis-loc
                       ?rover-loc
                       ?lander-loc))))
                (find 'hddl::have-line-of-sight-for-soil methods :key 'second)))))
