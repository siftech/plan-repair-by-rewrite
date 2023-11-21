;;;; Copyright (c) 2023 Robert P. Goldman and SIFT, LLC

(in-package #:plan-repair-rewrite)

(defvar *orig-plan*)
(defvar *orig-problem*)
(defvar *orig-domain*)
(defvar *erecord*)

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

    (let* ((*ordering-literals* (ordering-literals *erecord*))
           (*new-domain* (repair-domain *erecord* *ordering-literals*))
           (*new-problem* (repair-problem (problem *erecord*)
                                           (first *ordering-literals*)
                                           (car (last *ordering-literals*))
                                           (hddl-utils:domain-name *new-domain*))))
      ;; make sure all the old actions are retained
      (assert (null (set-difference (mapcar #'hddl-utils:action-name (hddl-utils:domain-actions *orig-domain*))
                                    (mapcar #'hddl-utils:action-name (hddl-utils:domain-actions *new-domain*)))))

      (with-open-file (str "repair-problem.hddl" :direction :output :if-exists :supersede)
        (hddl-io:pprint-hddl *new-problem* str))
      (with-open-file (str "repair-domain.hddl" :direction :output :if-exists :supersede)
        (hddl-io:pprint-hddl *new-domain* str)))))


#+nil
(defun plan-repair ()
  (let ((*default-pathname-defaults* (uiop:ensure-absolute-pathname
                                      (asdf:system-relative-pathname "plan-repair-rewrite"
                                                                     "data/"))))
    (pddl-planners:run-planner "repair-domain.hddl" "repair-problem.hddl" :panda)))
