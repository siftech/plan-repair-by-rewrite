(defpackage #:shop-plan-repair-rewrite
  (:use #:common-lisp #:iterate)
  (:export #:repair-domain-and-problem)
  (:import-from #:plan-repair-rewrite
                #:ordering-literals
                #:execution-record
                #:executed-actions
                #:unforeseen-pos
                #:unforeseen-neg)
  (:import-from #:shop
                #:problem
                #:domain
                #:domain-items)
  (:import-from #:alexandria
                #:when-let
                #:appendf
                #:deletef)
  (:import-from #:pddl-utils
                #:flatten-conjunction))

(in-package :shop-plan-repair-rewrite)

(defun method-task-network (method-sexp)
  (unless (and (= (length method-sexp)
               5)
               (member (first method-sexp) '(:method :pddl-method))
               (symbolp (third method-sexp)))                         ; keyword, task name preconditions task-network
    (error "Need rigid method structure of (:METHOD|:PDDL-METHOD TASK NAME PRECONDITIONS TASK-NETWORK)"))
  (fifth method-sexp))

(defsetf method-task-network (method-sexp) (task-net)
  `(setf (fifth ,method-sexp) ,task-net))
