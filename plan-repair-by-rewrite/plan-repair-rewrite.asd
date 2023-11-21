;;;; plan-repair-rewrite.asd
;;
;;;; Copyright (c) 2023 Robert P. Goldman (SIFT, LLC)

(defpackage :plan-repair-rewrite-asd
  (:use :common-lisp :asdf))

(in-package :plan-repair-rewrite-asd)


(defsystem #:plan-repair-rewrite
    :description "Implementatino of Hoeller et al.'s algorithm for plan repair by
rewriting."
  :author "Robert P. Goldman (SIFT, LLC)"
  :license  "BSD 3-clause"
  :version (:read-file-form "version.lisp-expr")
  :serial t
  :depends-on ("iterate"
               "alexandria"
               (:version "hddl-utils" "3")
               "shop3/unifier")
  :components ((:file "package")
               (:file "plan-repair-rewrite")))

(defsystem #:plan-repair-rewrite/shop3
  :description "Version of the plan-repair-rewrite algorithm adapted for SHOP3."
  :author "Robert P. Goldman (SIFT, LLC)"
  :license  "BSD 3-clause"
  :version (:read-file-form "version.lisp-expr")
  :serial t
  :depends-on ("plan-repair-rewrite"
               "shop3")
  :in-order-to ((test-op (test-op "plan-repair-rewrite/shop3-test")))
  :pathname "shop3/"
  :components ((:file "decls")
               (:file "plan-repair-rewrite")))

(defsystem #:plan-repair-rewrite/shop3-test
  :author "Robert P. Goldman (SIFT, LLC)"
  :license  "BSD 3-clause"
  :version (:read-file-form "version.lisp-expr"))
  :serial t
  :depends-on ("plan-repair-rewrite/shop3" "fiveam")
  :perform (test-op (op sys)
                    (let* ((test-results
                            (uiop:symbol-call :fiveam '#:run
                                              (uiop:intern* '#:shop-plan-repair-rewrite-tests
                                                            '#:test-shop-plan-repair-rewrite)))
                          (result (every #'(lambda (x) (typep x (uiop:intern* '#:test-passed '#:fiveam)))
                                         test-results)))
                      (if result
                          (format t "~&TEST-OP for plan-repair-rewrite/shop3 passed.~%")
                          (error "TEST-OP for plan-repair-rewrite/shop3 failed.~%Failing tests:~%~{~T~A~%~}"
                                 (mapcar #'(lambda (failure)
                                             (uiop:symbol-call '#:fiveam '#:name
                                                               (uiop:symbol-call
                                                                '#:fiveam '#:test-case
                                                                failure)))
                                         (remove-if #'(lambda (x) (typep x (uiop:intern* '#:test-passed '#:fiveam)))
                                             test-results))))))
  :pathname "shop3/"
  :components ((:file "tests")))
