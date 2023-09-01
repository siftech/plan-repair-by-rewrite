;;;; plan-repair-rewrite.asd
;;
;;;; Copyright (c) 2023 Robert P. Goldman (SIFT, LLC)


(asdf:defsystem #:plan-repair-rewrite
  :description "Describe plan-repair-rewrite here"
  :author "Robert P. Goldman (SIFT, LLC)"
  :license  "BSD 3-clause"
  :version "0.0.1"
  :serial t
  :depends-on ("iterate"
               "alexandria"
               (:version "hddl-utils" "3")
               "shop3/unifier")
  :components ((:file "package")
               (:file "plan-repair-rewrite")))
