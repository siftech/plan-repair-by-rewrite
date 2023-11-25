(in-package :plan-repair-rewrite)

(defparameter *hddl-plan*
  (hddl-utils:read-hddl-plan-file (merge-pathnames "original-plan.hddl" *load-truename*)))

(defparameter *bug-er*
  (let ((plan (getf
               (rest *hddl-plan*)
               :actions)))
    (make-instance 'prr:execution-record
                   :domain (hddl-utils:read-hddl-file "home:projects;hideho;domains;rovers;domain.hddl")
                   :problem (hddl-utils:read-hddl-file "home:projects;hideho;domains;rovers;p01.hddl")
                   :plan plan
                   :unforeseen-pos NIL
                   :unforeseen-neg 'hddl::((visible waypoint2 waypoint3))
                   :executed-actions (subseq plan 0 6)))
                 )

(defvar *repair-domain*)
(defvar *repair-problem*)

(multiple-value-setq (*repair-domain* *repair-problem*)
  (repair-domain-and-problem *bug-er*))
