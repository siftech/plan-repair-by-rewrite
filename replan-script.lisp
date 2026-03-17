;;;; Copyright (c) 2023 Robert P. Goldman and SIFT, LLC

(in-package #:plan-repair-rewrite)

(defvar *orig-plan*)
(defvar *orig-problem*)
(defvar *orig-domain*)
(defvar *erecord*)
(defvar *ordering-literals)
(defvar *new-domain*)
(defvar *new-problem*)

(defun setup-input-files ()
  (let ((*default-pathname-defaults* (uiop:ensure-absolute-pathname
                                    (asdf:system-relative-pathname "plan-repair-rewrite"
                                                                   "data/"))))

  (setf *orig-plan* (hddl-utils:read-hddl-plan-file "toll-plan.hddl"))
  (setf *orig-problem* (hddl-utils:read-hddl-file "toll-problem.hddl"))
  (setf *orig-domain* (hddl-utils:read-hddl-file "toll-domain.hddl"))
  (setf *erecord*
    (make-instance 'execution-record
                   :plan *orig-plan*
                   :problem *orig-problem*
                   :domain *orig-domain*
                   :executed-actions (subseq (getf (rest  *orig-plan*) :actions) 0 2)
                   :unforeseen-neg 'hddl::((road g f))
                   :unforeseen-pos nil))))



(defun build-repair-files ()
  (setup-input-files)
  (let ((*default-pathname-defaults* (uiop:ensure-absolute-pathname
                                      (asdf:system-relative-pathname "plan-repair-rewrite"
                                                                     "data/"))))
    (multiple-value-setq (*new-domain* *new-problem*)
      (repair-domain-and-problem *erecord*))

    ;; make sure all the old actions are retained
    (assert (null (set-difference (mapcar #'hddl-utils:action-name (hddl-utils:domain-actions *orig-domain*))
                                  (mapcar #'hddl-utils:action-name (hddl-utils:domain-actions *new-domain*)))))

    (with-open-file (str "repair-problem.hddl" :direction :output :if-exists :supersede)
      (hddl-io:pprint-hddl *new-problem* str))
    (with-open-file (str "repair-domain.hddl" :direction :output :if-exists :supersede)
      (hddl-io:pprint-hddl *new-domain* str))))


#+nil
(defun plan-repair ()
  (let ((*default-pathname-defaults* (uiop:ensure-absolute-pathname
                                      (asdf:system-relative-pathname "plan-repair-rewrite"
                                                                     "data/"))))
    (pddl-planners:run-planner "repair-domain.hddl" "repair-problem.hddl" :panda)))

(defparameter *rovers-dir*
  (uiop:ensure-absolute-pathname
   (translate-logical-pathname "home:projects;hideho;domains;rovers;")))

(defparameter *rovers-pathname-defaults*
  (make-pathname :name nil :directory (pathname-directory *rovers-dir*)
                 :type "hddl"))

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


(defun setup-rover-input-files ()
  (let ((*default-pathname-defaults* *rovers-pathname-defaults*))
    (setf *orig-domain* (hddl-utils:read-hddl-file "domain.hddl"))
    (setf *orig-problem* (hddl-utils:read-hddl-file "p01.hddl"))
    (setf *orig-plan* (let ((copy (copy-tree *shop-rover-plan*)))
                        (iter (for i from 1)
                          (as step in copy)
                          (collecting (cons i step)))))
    (setf *erecord*
          (make-instance 'execution-record
                         :plan *orig-plan*
                         :problem *orig-problem*
                         :domain *orig-domain*
                         :executed-actions (subseq *orig-plan* 0 6)
                         :unforeseen-neg (hddl-utils:hddlify-tree '((VISIBLE WAYPOINT3 WAYPOINT0)))
                         :unforeseen-pos nil))))

(defun build-rover-repair-files ()
  (setup-rover-input-files)
  (multiple-value-setq (*new-domain* *new-problem*)
      (repair-domain-and-problem *erecord*))
  (let ((*default-pathname-defaults* *rovers-pathname-defaults*))
    (uiop:with-output-file (str "repair-problem.hddl":if-exists :supersede)
      (hddl-utils:pprint-hddl *new-problem* str))
    (uiop:with-output-file (str "repair-domain.hddl" :if-exists :supersede)
      (hddl-utils:pprint-hddl *new-domain* str))))


#+nil
(defun rovers-plan-repair ()
  (let ((*default-pathname-defaults* *rovers-pathname-defaults*))
    (pddl-planners:run-planner "repair-domain.hddl" "repair-problem.hddl" :panda)))
