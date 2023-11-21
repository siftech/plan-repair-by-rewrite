;;;; Copyright (c) 2023 Robert P. Goldman and SIFT, LLC

(in-package :common-lisp-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (asdf:load-system "plan-repair-rewrite/shop3"))

(in-package :shop-plan-repair-rewrite)

(defvar *orig-plan*)
(defvar *orig-problem*)
(defvar *orig-domain*)
(defvar *erecord*)
(defvar *new-domain*)
(defvar *new-problem*)

(defun setup-input-files ()
  (let ((*default-pathname-defaults* (uiop:ensure-absolute-pathname
                                    (asdf:system-relative-pathname "plan-repair-rewrite"
                                                                   "data/"))))
    (load "toll-domain.lisp")
    (setf *orig-domain* (shop:find-domain 'shop-user::domain))
    (load "toll-problem.lisp")
    (setf *orig-problem* (shop:find-problem 'shop-user::problem))

    (setf *orig-plan* (uiop:with-input-file (str "toll-plan.lisp")
                        (let ((*package* (find-package :shop-user)))
                          (read str))))
  (setf *erecord*
    (make-instance 'execution-record
                   :plan *orig-plan*
                   :problem *orig-problem*
                   :domain *orig-domain*
                   :executed-actions (subseq *orig-plan* 0 2)
                   :unforeseen-neg 'shop-user::((road g f))
                   :unforeseen-pos nil))))



(defun build-repair-files ()
  (setup-input-files)
  (let ((*default-pathname-defaults* (uiop:ensure-absolute-pathname
                                      (asdf:system-relative-pathname "plan-repair-rewrite"
                                                                     "data/"))))

    (let ((ordering-literals (ordering-literals *erecord* :package :shop-user)))
      (setf *new-domain* (repair-domain *erecord* ordering-literals))
      (setf *new-problem* (repair-problem (er-problem *erecord*)
                                          (first ordering-literals)
                                          (car (last ordering-literals))
                                          ;; domain name
                                          (second *new-domain*)))
      (with-open-file (str "repair-problem.lisp" :direction :output :if-exists :supersede)
        (format str "(in-package :SHOP-USER)~%")
        (let ((*package* (find-package :shop-user)))
          (write *new-problem* :stream str :readably t)))
      (with-open-file (str "repair-domain.lisp" :direction :output :if-exists :supersede)
        (format str "(in-package :SHOP-USER)~%")
        (let ((*package* (find-package :shop-user)))
          (write *new-domain* :stream str :readably t))))))


(defun plan-repair (&key (problem-file "repair-problem.lisp"))
  (let ((*default-pathname-defaults* (uiop:ensure-absolute-pathname
                                      (asdf:system-relative-pathname "plan-repair-rewrite"
                                                                     "data/"))))
    (load "repair-domain.lisp")
    (load problem-file)
    ;; last problem defined...
    (shop3:find-plans-stack shop::*problem*)))

(defparameter *shop-rover-plan*
  'shop-rovers::((!NAVIGATE ROVER0 WAYPOINT3 WAYPOINT1)
                   (!NAVIGATE ROVER0 WAYPOINT1 WAYPOINT2)
                   (!SAMPLE_SOIL ROVER0 ROVER0STORE WAYPOINT2)
                   (!COMMUNICATE_SOIL_DATA ROVER0 GENERAL WAYPOINT2 WAYPOINT2 WAYPOINT0)
                   (!NAVIGATE ROVER0 WAYPOINT2 WAYPOINT1)
                   (!NAVIGATE ROVER0 WAYPOINT1 WAYPOINT3)
                   (!DROP ROVER0 ROVER0STORE)
                   (!SAMPLE_ROCK ROVER0 ROVER0STORE WAYPOINT3)
                   (!COMMUNICATE_ROCK_DATA ROVER0 GENERAL WAYPOINT3 WAYPOINT3 WAYPOINT0)
                   (!NAVIGATE ROVER0 WAYPOINT3 WAYPOINT0)
                   (!CALIBRATE ROVER0 CAMERA0 OBJECTIVE1 WAYPOINT0)
                   (!TAKE_IMAGE ROVER0 WAYPOINT0 OBJECTIVE1 CAMERA0 HIGH_RES)
                   (!NAVIGATE ROVER0 WAYPOINT0 WAYPOINT3)
                   (!NAVIGATE ROVER0 WAYPOINT3 WAYPOINT1)
                   (!COMMUNICATE_IMAGE_DATA ROVER0 GENERAL OBJECTIVE1 HIGH_RES WAYPOINT1 WAYPOINT0)))

(defparameter *rovers-dir*
  (uiop:ensure-absolute-pathname
   (translate-logical-pathname "home:projects;hideho;domains;rovers;")))


(defun setup-rover-input-files ()
  (let ((*default-pathname-defaults* *rovers-dir*))
    (load "domain.lisp")
    (setf *orig-domain* (shop:find-domain 'shop-rovers::rover))
    (load "p01.lisp")
    (setf *orig-problem* (shop::find-problem 'shop-rovers::ROVERPROB01))
    (setf *orig-plan* (copy-tree *shop-rover-plan*))
    (setf *erecord*
          (make-instance 'execution-record
                         :plan *orig-plan*
                         :problem *orig-problem*
                         :domain *orig-domain*
                         :executed-actions (subseq *orig-plan* 0 6)
                         :unforeseen-neg 'shop-rovers::((VISIBLE WAYPOINT3 WAYPOINT0))
                         :unforeseen-pos nil))))



(defun build-rover-repair-files ()
  (setup-rover-input-files)
  (let ((*default-pathname-defaults* *rovers-dir*))

    (let ((ordering-literals (ordering-literals *erecord* :package :shop-user)))
      (setf *new-domain* (repair-domain *erecord* ordering-literals))
      (setf *new-problem* (repair-problem (er-problem *erecord*)
                                          (first ordering-literals)
                                          (car (last ordering-literals))
                                          ;; domain name
                                          (let ((domain-name-or-name-and-opts
                                                  (second *new-domain*)))
                                            (etypecase domain-name-or-name-and-opts
                                              (symbol domain-name-or-name-and-opts)
                                              (list (first domain-name-or-name-and-opts))))))
      (with-open-file (str "repair-problem.lisp" :direction :output :if-exists :supersede)
        (format str "(in-package :SHOP-USER)~%")
        (let ((*package* (find-package :shop-user)))
          (write *new-problem* :stream str :readably t)))
      (with-open-file (str "repair-domain.lisp" :direction :output :if-exists :supersede)
        (format str "(in-package :SHOP-USER)~%")
        (let ((*package* (find-package :shop-user)))
          (write *new-domain* :stream str :readably t))))))


(defun rovers-plan-repair (&key (problem-file "repair-problem.lisp"))
  (let ((*default-pathname-defaults* *rovers-dir*))
    (load "repair-domain.lisp")
    (load problem-file)
    ;; last problem defined...
    (shop3:find-plans-stack shop::*problem*)))
