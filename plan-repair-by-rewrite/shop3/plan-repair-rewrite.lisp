;;;; plan-repair-rewrite.lisp
;;
;;;; Copyright (c) 2023 Robert P. Goldman and SIFT, LLC





(in-package #:shop-plan-repair-rewrite)

(defun er-domain (execution-record)
  (prr::domain execution-record))

(defun er-problem (execution-record)
  (prr::problem execution-record))

(deftype only-values (&rest value-spec)
  `(values ,@value-spec &optional))

(deftype only-value (value-spec)
  `(values ,value-spec &optional))

#-allegro
(declaim (ftype (function (execution-record)
                          (only-values list
                                       list))
                repair-domain-and-problem))

(defun repair-domain-and-problem (er)
  "Return two s-expressions, one a SHOP domain definition, and the
other a SHOP problem definition."
  (let* ((problem (er-problem er))
         (ordering-literals (ordering-literals er :package :shop-user))
         (repair-domain-sexp (repair-domain er ordering-literals)))
    (values repair-domain-sexp
            (repair-problem problem (first ordering-literals)
                            (alexandria:last-elt ordering-literals)
                            ;; domain-name -- this is yucky
                            (second repair-domain-sexp)))))

(defun domain-primitives (item-list)
  "Return list of all primitives defined in ITEM-LIST."
  (remove 'shop::!!inop
   (remove-if-not #'(lambda (x) (member (first x) '(:action :operator :op)))
                  item-list)
   :key #'primitive-name))

(defun domain-methods (item-list)
  "Return list of all primitives defined in ITEM-LIST."
  (remove-if-not #'(lambda (x) (member (first x) '(:method :pddl-method)))
                 item-list))

(defun domain-items-no-primitives-or-methods (domain-items)
  (remove-if #'(lambda (x) (member (first x) '(:action :operator :op :method :pddl-method)))
             domain-items))


;;; For each task ai with 1 ≤ i < m−1 in the prefix of executed
;;; actions, a new task name a′i is introduced with prec′(a′i) 7→
;;; prec(ai) ∪ {li−1}, add′(a′i) 7→ add(ai) ∪ {li} and del′(a′i) 7→
;;; del(ai) ∪ {li−1}.
(declaim (ftype (function (t execution-record)
                          (values list t list &optional))
                new-primitives))
(defun new-primitives (ordering-literal-list exec-record)
  "Create a set of new primitives for the domain in EXEC-RECORD.  Return three values:
1. LIST of new primitive definitions
2. LIST of old action names
3. Full set of new PRIMITIVES -- the new ones for repair and the modified ones."
  (let* ((new-domain (copy-tree (domain-items (er-domain exec-record))))
         (last-literal (list (first (last ordering-literal-list))))
         (modified-actions (modified-primitives (domain-primitives new-domain) last-literal)))
    (multiple-value-bind (new-primitives old-action-names)
        (make-new-primitives (executed-actions exec-record) ordering-literal-list
                             (domain-primitives new-domain)
                             exec-record)
      (values new-primitives old-action-names (append modified-actions new-primitives)))))

#-allegro
(declaim (ftype (function (list)
                   (only-values list))
                primitive-precond))
(defun primitive-precond (prim)
  "Return the precondition extracted from PRIM, which might be an
:OPERATOR, :OP, or :ACTION form."
  (ecase (first prim)
    (:operator (canonicalize-conjunction (third prim)))
    (:op (getf prim :precond))
    (:action (getf prim :precondition))))

(defsetf primitive-precond (prim) (precond)
  `(ecase (first ,prim)
     (:operator (setf (third ,prim) (canonicalize-conjunction ,precond)))
     (:op (setf (getf ,prim :precond) ,precond))
     (:action (setf (getf ,prim :precondition) ,precond))))

(defun canonicalize-conjunction (expr)
  (check-type expr list)
  (cond ((symbolp (first expr))
         ;; this is either a conjunction or a literal
         expr)
        ((listp (first expr))
         `(and ,@expr))))

(defun primitive-parameters (prim)
  "Return the parameter list extracted from PRIM, which might be an
:OPERATOR, :OP, or :ACTION form. Should be *without type qualifiers*"
  (ecase (first prim)
    ((:operator :op) (rest (second prim)))
    (:action (hddl-utils:remove-types-from-list (getf prim :parameters)))))


(defun primitive-name (prim)
  "Return the precondition extracted from PRIM, which might be an
:OPERATOR, :OP, or :ACTION form."
  (ecase (first prim)
    ((:operator :op) (first (second prim)))
    (:action (second prim))))

(defsetf primitive-name (prim) (name)
  `(ecase (first ,prim)
     ((:op :operator) (setf (second ,prim)
                            (cons ,name (rest (second ,prim)))))
     (:action (setf (second ,prim) ,name))))


(defun modified-primitives (primitives last-literal)
  "Return a modified list of PRIMITIVES with the LAST-LITERAL added to the
precondition of each primitive. Destructively modifies its argument.
  This is used to implement the part of the repair algorithm that forces
all normal/new primitives to occur after the executed prefix."
  (iter (for primitive in primitives)
    (as new-primitive = (copy-tree primitive))
    (setf (primitive-precond new-primitive)
          `(and ,(primitive-precond new-primitive)
                ,(copy-list last-literal)))
    (collecting new-primitive)))

(defun make-new-primitives (executed ordering-literal-list old-prims exec-record)
  "Generates and returns a set of new primitive definitions used to force the re-execution of the
executed prefix of the plan (EXECUTED), using the ORDERING-LITERAL-LIST in preconditions
to get the correct sequencing."
  (iter (for (prim next-prim . nil) on executed) ; ground actions
    (as index from 0)
    (as (ordering-literal next-ordering-literal . nil) on ordering-literal-list)
    (as prim-name = (first prim))
    ;; the action index must be part of the name to ensure it's unique -- the task name is not sufficient.
    (as new-prim-name = (intern (format nil "~A-~D-PRIME" prim-name index)
                                (symbol-package prim-name)))
    (as old-prim-def = (or (find prim-name old-prims
                                   :key #'primitive-name)
                             (error "Can't find previous definition of primitive ~s in domain"
                                    prim-name)))
    (declare  (type integer index) (type list prim old-prim-def) (type symbol prim-name new-prim-name))
    (collecting (make-new-prim prim new-prim-name old-prim-def
                 ordering-literal next-ordering-literal
                 :last-p (null next-prim)
                 :er exec-record)
      into new-prims)
    (collecting prim-name into old-prim-names)
    (finally (return (values new-prims old-prim-names)))))

;;; new actions and methods that generate only the actions from
;;; the original plan.
;;; C′ =C∪{c′a |a∈A},c′a ̸∈C∪A,
;;; Ma = {(c′a,({t},∅,{t 7→ a})) | ∀a ∈ A}
(defun new-action-tasks-and-methods (new-action-list old-action-names all-actions)
  ;; regroup the new-action list by old-action-names
  (let ((alist (regroup-action-lists new-action-list old-action-names)))
    (iter (for (old-name . new-actions) in alist)
      (as old-action = (or
                      (find old-name all-actions :key #'primitive-name)
                      (error "Couldn't find a previous action definition for action: ~s" old-name)))
      (collecting old-name into old-names)
      (multiple-value-bind (new-task methods)
          (new-action-task-and-methods new-actions old-name old-action)
        (collecting new-task into new-tasks)
        (appending methods into new-methods)
        (finally (return (values old-names new-tasks new-methods)))))))


(defun new-action-task-and-methods (new-action-defs old-action-name old-action)

  "Takes old SHOP or HDDL action definitions NEW-ACTION-DEFS for a
  replay action, and an OLD-ACTION-NAME (a symbol) and makes a new
  task and methods so that the new task can be expanded into either
  one of the NEW-ACTION-DEFS or the OLD-ACTION.  Returns 1. New task
  definition 2. New methods that expand to either a new action or the
  original one."

  (declare (type list new-action-defs old-action) (type symbol old-action-name))
  (let* ((new-task-name (intern* (concatenate 'string "PLAY-OR-REPLAY-"
                                                   (string-left-trim (list #\!) (string old-action-name)))
                                      (symbol-package old-action-name)))
         (new-task `(,new-task-name ,@(primitive-parameters old-action)))
         methods)
    ;; expand NEW-TASK as a new primitive action, not a replay
    (push `(:method ;; task-sexpr
             ,new-task ;; name
             ,(intern (concatenate 'string (string-left-trim (list #\!) (string old-action-name))
                                   "-METHOD")
                      (symbol-package old-action-name))
             () ; no preconditions
             (,old-action-name ,@(primitive-parameters old-action)))
          methods)
    (iter (for new-action-def in new-action-defs)
      (as prim-name = (primitive-name new-action-def))
      (as prim-name-no-bang = (string-left-trim (list #\!) (string prim-name)))
      (as prim-params = (primitive-parameters new-action-def))
      (as new-name-replay = (gentemp prim-name-no-bang
                                     (symbol-package prim-name)))
      (declare (type symbol prim-name new-name-replay)
               (type string prim-name-no-bang))
      (push `(:method
               ;; task-sexpr
                 (,new-task-name ,@prim-params)
               ,new-name-replay
               () ; no preconditions
               ;; task network
               (,prim-name ,@prim-params))
            methods))
    (values new-task methods)))


(defun add-new-delete (primitive delete)
  "Update the PRIMITIVE to delete DELETE as an effect.  Return the
  PRIMITIVE, which is destructively modified."
  (ecase (first primitive)
    (:operator (setf (fourth primitive) (cons delete (fourth primitive))))
    (:op (setf (getf primitive :delete)
               (cons delete (getf primitive :delete))))
    (:action (setf (getf primitive :effect)
                   `(and ,(getf primitive
                                :effect)
                         (not ,delete)))))
  primitive)

(defun add-new-add (primitive add)
   "Update the PRIMITIVE to add ADD as an effect.  Return the
    PRIMTIIVE, which is destructively modified."
  (ecase (first primitive)
    (:operator (setf (fifth primitive)
                     (cons add (fifth primitive))))
    (:op (setf (getf primitive :add)
               (cons add (getf primitive :add))))
    (:action (setf (getf primitive :effect)
                   `(and ,(getf primitive :effect) ,add))))
  primitive)

(defun make-new-prim (executed new-prim-name old-prim ordering-literal
                      next-ordering-literal &key last-p er)
  "Return a new primitive for replaying the EXECUTED action."
  (when last-p
    (unless er
      (error "Need to supply an execution record with the last primitive spec.")))
  (let* ((new-prim (copy-tree old-prim))
         (new-delete `(,ordering-literal))
         (new-add `(,next-ordering-literal))
         (bindings
           (prim-bindings new-prim executed))
         binding-preconds)
    (ecase (first new-prim)
      (:action (setf binding-preconds
                     (iter (for binding in bindings)
                       (as var = (shop3.unifier:binding-var binding))
                       (as val = (shop3.unifier:binding-val binding))
                       (collecting `(= ,var ,val)))))
      ((:op :operator)
       ;; SHOP primitives can have task arguments that are constants
       (setf new-prim (shop.unifier:apply-substitution new-prim bindings))))
    (setf (primitive-precond new-prim)
          `(and
            (,ordering-literal)
            ,@binding-preconds
            ,(primitive-precond new-prim)))

    (setf (primitive-name new-prim) new-prim-name)
    (add-new-delete new-prim new-delete)
    (add-new-add new-prim new-add)
    (when last-p
      (new-act-revised-effects new-prim er))
    new-prim))


(defun prim-bindings (primitive-def executed-task)
  "Return a binding list that can be used to instantiate PRIMITIVE-DEF
to match the parameterization (grounding) of EXECUTED-TASK."
  (let ((params (primitive-parameters primitive-def))
        (bindings (rest executed-task)))
    (shop3.unifier:make-binding-list params bindings)))

(defun new-act-revised-effects (new-prim er)
  ;; The last action in the executed prefix am needs to have additional
  ;; effects, it performs the unforeseen state change. prec′(a′m) 7→
  ;; prec(am) ∪ {lm−1}, add′(a′m) 7→ (add(am) \ F−) ∪ F+ ∪ {lm} and
  ;; del′(a′m) 7→ del(am) ∪ F− ∪ {lm−1}. The original problem is placed
  ;; after the prefix, i.e., ∀a ∈ A holds that prec′(a) 7→ prec(a) ∪
  ;; {lm}. And the new set of actions is definedas A′ = A∪{a′i |1≤i≤m}.
  (with-slots (unforeseen-pos unforeseen-neg) er
    (iter (for x in unforeseen-pos)
      (add-new-add new-prim x))
    (iter (for x in unforeseen-neg)
      (add-new-delete new-prim x))))

;;;To make the first action of the prefix applicable in the initial
;;;state, the symbol l0 is added, i.e., s′0 = s0 ∪ {l0}. To reuse the
;;;already executed actions, ensure that every solution starts with
;;;the entire prefix, i.e. g′ = g ∪ {lm}.
(defun repair-problem (problem literal-zero literal-n domain-name)
  (let ((problem-state (copy-tree (shop::problem-state problem)))
        (problem-tasks (copy-tree (shop::problem-tasks problem)))
        (check-task (check-last-ordering-literal-task-name literal-n))
        (problem-name
          (let ((old-name (shop::problem-name problem)))
            (intern (concatenate 'string (symbol-name old-name) "-REPAIR")
                    (symbol-package old-name)))))
    `(shop:defproblem ,problem-name ,domain-name
       ,(append problem-state
                `((,literal-zero)))
       (:ordered ,problem-tasks
                 (,check-task)))))

(defun check-last-ordering-literal-task-name (literal-n)
  (intern (concatenate 'string "!!CHECK-" (symbol-name literal-n))
                    (symbol-package literal-n)))

(defun check-last-ordering-op-definition (literal-n)
  "Return definition of a new operator that will check to ensure that
LITERAL-N -- the symbol that names the predicate that is added when
the prefix of tasks has been completed -- is in the final state."
  `(:op (,(check-last-ordering-literal-task-name literal-n))
    :precond (,literal-n)))

(defun repair-domain (er ordering-literals)
  "Return an S-EXPRESSION that defines the repair domain."
  (multiple-value-bind (new-primitives old-action-names all-primitives)
      (new-primitives ordering-literals er)
    (multiple-value-bind (old-action-names new-complex-tasks new-methods)
        (new-action-tasks-and-methods new-primitives old-action-names all-primitives)
      (let* ((literal-n (first (last ordering-literals)))
             (new-domain-items (append all-primitives
                                       (list (check-last-ordering-op-definition literal-n))
                                       new-methods
                                       (revise-original-methods (domain-methods (domain-items (er-domain er)))
                                                                old-action-names new-complex-tasks)
                                       (domain-items-no-primitives-or-methods (domain-items (er-domain er)))))
             (orig-domain-name (shop::domain-name (er-domain er)))
             (new-domain-name (intern* (concatenate 'string (symbol-name orig-domain-name) "-REPAIR")
                                            (symbol-package orig-domain-name))))
        ;; need an equality axiom
        (unless (find-if #'(lambda (item) (and (eq (first item) :-) (eq (first (second item)) '=)))
                         new-domain-items)
          (alexandria:nconcf new-domain-items '((:- (= shop-user::?x shop-user::?x) ()))))
        `(SHOP:defdomain (,new-domain-name :type ,(class-name (class-of (er-domain er))))
           ,new-domain-items)))))

(defun revise-original-methods (methods old-action-names new-complex-tasks)
  "Return a revised set of methods based on METHODS, but with references to
OLD-ACTION-NAMES in their subtasks replaced by references to NEW-COMPLEX-TASKS."
  (let ((new-methods (copy-tree methods))
        (new-complex-task-names (mapcar #'shop::get-task-name new-complex-tasks)))
    (iter (for new-method in new-methods)
      (as subtasks = (copy-tree (method-task-network new-method)))
      (iter (for old-action-name in old-action-names)
        (as new-task-name in new-complex-task-names)
        (setf subtasks (subst new-task-name old-action-name subtasks)))
      (setf (method-task-network new-method) subtasks))
    new-methods))
