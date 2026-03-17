;;;; plan-repair-rewrite.lisp
;;
;;;; Copyright (c) 2023 Robert P. Goldman and SIFT, LLC


(in-package #:plan-repair-rewrite)

(defclass execution-record ()
  ((executed-actions
    :initarg :executed-actions
    :reader executed-actions
    :documentation "Sequence of previously-executed actions.
It will be an ALIST of (integer . ground task expression)*"
    )
   (unforeseen-pos
    :initarg :unforeseen-pos
    :reader unforeseen-pos
    :documentation "Facts unexpectedly becoming true."
    )
   (unforeseen-neg
    :initarg :unforeseen-neg
    :reader unforeseen-neg
    :documentation "Facts unexpectedly becoming false."
    )
   (plan
    :initarg :plan
    :reader plan
    )
   (problem
    :initarg :problem
    :reader problem)
   (domain
    :initarg :domain
    :reader domain
    )))

(declaim (ftype (function (execution-record)
                          (values list  ; domain
                                  list  ; problem
                                  &optional))
                repair-domain-and-problem))

(defun repair-domain-and-problem (er)
  (with-slots (problem domain) er
    (let* ((ordering-literals (ordering-literals er))
           (repair-domain (repair-domain er ordering-literals)))
      (values repair-domain
              (repair-problem problem (first ordering-literals)
                              (alexandria:last-elt ordering-literals)
                              (hddl-utils:domain-name repair-domain))))))

;;; First, a sequence of new propositional symbols is introduced
;;; that indicate the position of some action in the enforced plan
;;; prefix. We denote these facts by li with 0 ≤ i ≤ m and li ̸∈ L and
;;; define the new set of propositional state features as
;;; L′ = L ∪ {li |0≤i≤m}.
(defgeneric ordering-literals (info &key package)
  (:documentation "Return the set of ordering literal names for INFO.")
  (:method ((er execution-record) &key (package :hddl))
    (ordering-literals (executed-actions er) :package package))
  (:method (acts &key (package :hddl))
    (iter (with sym-name = "L")
      ;; there is one extra ordering literal -- the one for before the first action.
      (for i from 0 to (length acts))
      (collecting
          (intern (format nil "~a-~d" sym-name i) package)))))




;;; For each task ai with 1 ≤ i < m−1 in the prefix of executed
;;; actions, a new task name a′i is introduced with prec′(a′i) 7→
;;; prec(ai) ∪ {li−1}, add′(a′i) 7→ add(ai) ∪ {li} and del′(a′i) 7→
;;; del(ai) ∪ {li−1}.
(declaim (ftype (function (t execution-record)
                          (values t t t &optional))
                new-actions))
(defun new-actions (ordering-literal-list exec-record)
  (let ((hddl-utils:*pddl-package* hddl-io:*hddl-package*))
    (let* ((domain (copy-tree (domain exec-record)))
           (new-domain (hddl-utils:canonicalize-domain domain))
           (last-literal (list (first (last ordering-literal-list))))
           (modified-actions (modified-actions (domain-actions domain) last-literal)))
      (multiple-value-bind (new-actions old-action-names)
          (make-new-actions (executed-actions exec-record) ordering-literal-list
                            (hddl-utils:domain-actions domain)
                            exec-record)

        (let* ((orig-domain-name (hddl-utils:domain-name domain))
               (new-domain-name (hddl-utils:hddl-symbol (concatenate 'string (symbol-name orig-domain-name) "-REPAIR"))))
          (setf (hddl-utils:domain-name new-domain) new-domain-name))
        (setf (hddl-utils:domain-actions new-domain) (append modified-actions new-actions))
        (values new-actions old-action-names new-domain)))))

(defun modified-actions (actions last-literal)
  "Return a modified list of ACTIONS with the LAST-LITERAL added to the
precondition of each action. Destructively modifies its argument.
  This is used to implement the part of the repair algorithm that forces
all normal/new actions to occur after the executed prefix."
  (iter (for action in actions)
    (as new-action = (copy-tree action))
    (setf (hddl-utils:action-precondition new-action)
          `(and ,(hddl-utils:action-precondition new-action)
                ,(copy-list last-literal)))
    (collecting new-action)))

(defun make-new-actions (executed ordering-literal-list old-actions exec-record)
  "Generates and returns a set of new action definitions used to force the re-execution of the
executed prefix of the plan (EXECUTED), using the ORDERING-LITERAL-LIST in preconditions
to get the correct sequencing."
  (iter (for ((index . action) next-act . nil) on executed) ; ground actions
    (as (ordering-literal next-ordering-literal . nil) on ordering-literal-list)
    (as action-name = (first action))
    ;; the action index must be part of the name to ensure it's unique -- the task name is not sufficient.
    (as new-action-name = (intern (format nil "~A-~D-PRIME" action-name index)
                                  :hddl))
    (as old-action-def = (or (find action-name old-actions
                                   :key #'hddl-utils:action-name)
                             (error "Can't find previous definition of action ~s in domain named ~s"
                                    action-name (hddl-utils:domain-name (domain exec-record)))))
    (declare (type integer index) (type list action old-action-def) (type symbol action-name new-action-name))
    (collecting (make-new-action action new-action-name old-action-def
                 ordering-literal next-ordering-literal
                 :last-p (null next-act)
                 :er exec-record)
      into new-actions)
    (collecting action-name into old-action-names)
    (finally (return (values new-actions old-action-names)))))

;;; new actions and methods that generate only the actions from
;;; the original plan.
;;; C′ =C∪{c′a |a∈A},c′a ̸∈C∪A,
;;; Ma = {(c′a,({t},∅,{t 7→ a})) | ∀a ∈ A}
(defun new-action-tasks-and-methods (new-action-list old-action-names all-actions)
  (let ((alist (regroup-action-lists new-action-list old-action-names)))
    (iter (for (old-name . new-actions) in alist)
      (as old-action = (or
                        (find old-name all-actions :key #'hddl-utils:action-name)
                        (error "Couldn't find a previous action definition for action: ~s" old-name)))
      (collecting old-name into old-names)
      (multiple-value-bind (new-task methods)
          (new-action-task-and-methods new-actions old-name old-action)
        (collecting new-task into new-tasks)
        (appending methods into new-methods)
        (finally (return (values old-names new-tasks new-methods)))))))

(defun new-action-task-and-methods (new-action-defs old-action-name old-action &aux methods)
  "Takes an HDDL action definition NEW-ACTION-DEF for a replay action,
and an OLD-ACTION-NAME (a symbol) and makes a new task and methods so that
the new task can be expanded into either NEW-ACTION-DEF or the original
action.
  Returns two values:
1. New task definition and
2. New methods that expand to either the new action or the original one."
  (declare (type list new-action-defs old-action) (type symbol old-action-name))
  (let* ((new-task-name (hddl-utils:hddl-symbol
                         (concatenate 'string
                                      "PLAY-OR-REPLAY-"
                                      (symbol-name old-action-name))))
         (new-task (hddl-utils::make-complex-task new-task-name
                                         (hddl-utils:action-params old-action))))
    ;; expand NEW-TASK as a new primitive action, not a replay
    (let* ((new-name (gentemp (symbol-name old-action-name) :hddl))
           (new-method-play (hddl-utils::make-ordered-method
                               new-name
                               ;; task-sexpr
                               `(,new-task-name
                                 ,@(hddl-utils:remove-types-from-list
                                    (hddl-utils::task-parameters new-task)))
                               ;; params
                               (hddl-utils::task-parameters new-task)
                               ;; task-network
                               :tasks
                               `((,old-action-name
                                  ,@(hddl-utils:remove-types-from-list
                                     (hddl-utils::task-parameters new-task)))))))
      (push new-method-play methods))
    (iter (for new-action-def in new-action-defs)
      (as prim-name = (hddl-utils:action-name new-action-def))
      (as new-name-replay = (gentemp (symbol-name old-action-name) :hddl))
      (declare (type symbol prim-name new-name-replay))
      (push (hddl-utils::make-ordered-method
             new-name-replay
             ;; task-sexpr
             `(,new-task-name
               ,@(hddl-utils:remove-types-from-list
                  (hddl-utils::task-parameters new-task)))
             ;; params
             (hddl-utils:task-parameters new-task)
             :precond (copy-tree (hddl-utils:action-precondition new-action-def))
             ;; task-network
             :tasks
             `((,prim-name
                ,@(hddl-utils:remove-types-from-list
                   (hddl-utils::task-parameters new-task)))))
            methods))
    (values new-task methods)))

(defun make-new-action (executed-task new-action-name old-action ordering-literal next-ordering-literal
                        &key last-p er)
  (when last-p
    (unless er (error "Need to supply an execution record with the last action spec.")))
  (hddl-utils:make-action new-action-name
                          (copy-tree (hddl-utils:action-params old-action))
                          :precondition
                          `(and ,@(prim-bindings old-action executed-task),
                                (copy-tree (hddl-utils:action-precondition old-action))
                                (,ordering-literal))
                          :effect
                          (if last-p
                              (new-act-revised-effects old-action ordering-literal next-ordering-literal er)
                              `(and ,(copy-tree (hddl-utils:action-effect old-action))
                                    (not (,ordering-literal))
                                    (,next-ordering-literal)))))

(defun prim-bindings (primitive-def executed-task)
  "Return a set of equality literals that enforce that the ACTION whose definition is PRIMITIVE-DEF
will be executed with the same arguments as EXECUTED-TASK by checking that the parameters of
PRIMITIVE-DEF are bound to the same values as before."
  (let ((params (hddl-utils:remove-types-from-list (hddl-utils:action-params primitive-def)))
        (bindings (rest executed-task)))
    (iter (for param in params)
      (as binding in bindings)
      (collecting `(= ,param ,binding)))))

(defun new-act-revised-effects (old-action ordering-literal next-ordering-literal er)
  ;; The last action in the executed prefix am needs to have additional
  ;; effects, it performs the unforeseen state change. prec′(a′m) 7→
  ;; prec(am) ∪ {lm−1}, add′(a′m) 7→ (add(am) \ F−) ∪ F+ ∪ {lm} and
  ;; del′(a′m) 7→ del(am) ∪ F− ∪ {lm−1}. The original problem is placed
  ;; after the prefix, i.e., ∀a ∈ A holds that prec′(a) 7→ prec(a) ∪
  ;; {lm}. And the new set of actions is definedas A′ = A∪{a′i |1≤i≤m}.
  (assert next-ordering-literal)        ; cannot be NIL
  (with-slots (unforeseen-pos unforeseen-neg) er
    ;; effects-conj does not include the AND -- put it back before returning.
    (let ((effects-conj (if (eq (first (hddl-utils:action-effect old-action)) 'and)
                            (flatten-conjunction
                             (hddl-utils:action-effect old-action))
                            (hddl-utils:action-effect old-action))))
      (iter (for x in unforeseen-pos)
        (as to-remove = `(not ,x))
        (deletef effects-conj to-remove  :test 'equalp))
      (iter (for x in unforeseen-neg)
        (deletef effects-conj x :test 'equalp))
      `(and ,effects-conj
            (,next-ordering-literal)
            (not (,ordering-literal))
            ,@(when unforeseen-neg (mapcar #'(lambda (x) `(not ,x)) unforeseen-neg))
            ,@(when unforeseen-pos (copy-tree unforeseen-pos))))))


;;;To make the first action of the prefix applicable in the initial
;;;state, the symbol l0 is added, i.e., s′0 = s0 ∪ {l0}. To reuse the
;;;already executed actions, ensure that every solution starts with
;;;the entire prefix, i.e. g′ = g ∪ {lm}.
(defun repair-problem (problem literal-zero literal-n domain-name)
  (let ((new-problem (hddl-utils:copy-problem problem)))
    (setf (hddl-utils:problem-name problem)
          (let ((old-name (hddl-utils:problem-name problem)))
            (intern (concatenate 'string (symbol-name old-name) "-REPAIR")
                    (symbol-package old-name))))
    (alexandria:appendf (hddl-utils:problem-state new-problem)
                        `((,literal-zero)))
    (setf (hddl-utils:problem-goal new-problem)
     (if (hddl-utils:problem-goal problem)
         `(and ,(hddl-utils:problem-goal problem)
               (,literal-n))
         `(,literal-n)))
    (setf (hddl-utils:problem-domain new-problem) domain-name)
    new-problem))

(defun repair-domain (er ordering-literals)
  (multiple-value-bind (new-actions old-action-names new-domain)
      (new-actions ordering-literals er)
    ;; define the new symbols as predicates with no
    ;; parameters
    (appendf (hddl-utils:domain-predicates new-domain)
             (mapcar #'list ordering-literals))
    (multiple-value-bind (old-action-names new-complex-tasks new-methods)
        (new-action-tasks-and-methods new-actions old-action-names (hddl-utils:domain-actions (domain er)))
      (appendf (hddl-utils:domain-tasks new-domain)
               new-complex-tasks)
      (setf (hddl-utils:domain-methods new-domain)
            (append (revise-original-methods (hddl-utils:domain-methods new-domain)
                                             old-action-names new-complex-tasks)
                    new-methods)))
    new-domain))

(defun revise-original-methods (methods old-action-names new-complex-tasks)
  "Return a revised set of methods based on METHODS, but with references to
OLD-ACTION-NAMES in their subtasks replaced by references to NEW-COMPLEX-TASKS."
  (assert (= (length old-action-names) (length new-complex-tasks)))
  (let ((new-methods (copy-tree methods))
        (new-complex-task-names (mapcar #'hddl-utils:task-name new-complex-tasks)))
    (dolist (new-method new-methods)
      (let ((subtasks (copy-tree (hddl-utils:method-subtasks new-method))))
        (iter (for old-action-name in old-action-names)
          (as new-task-name in new-complex-task-names)
          (setf subtasks (subst new-task-name old-action-name subtasks)))
        (setf (hddl-utils:method-subtasks new-method) subtasks)))
    new-methods))

;;; the NEW-ACTION-LIST contains new operator and action definitions
;;; created to replay actions in the executed part of the plan.  The
;;; corresponding original action names are in OLD-ACTION-NAMES.  This
;;; function regroups these into an ALIST whose entries are of this
;;; form:
(defun regroup-action-lists (new-action-list old-action-names)
  (let (alist)
    (iter (for new-action in new-action-list)
      (as old-name in old-action-names)
      (push new-action (alexandria:assoc-value alist
                                               old-name)))
    alist))
