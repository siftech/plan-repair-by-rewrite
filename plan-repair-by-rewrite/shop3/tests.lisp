(defpackage :test-shop-plan-repair-rewrite
  (:use :common-lisp :iterate :fiveam)
  (:import-from :shop-plan-repair-rewrite
                #:make-new-prim
                #:primitive-precond
                #:primitive-name))

(in-package :test-shop-plan-repair-rewrite)


(def-suite* shop-plan-repair-rewrite-tests)

(test extract-action-precond
  (let ((act '(:action pick-up
               :parameters
               (?arm - bot
                ?cupcake - locatable
                ?loc - location)
               :precondition
               (and
                (on ?arm ?loc)
                (on ?cupcake ?loc)
                (arm-empty)
                )
               :effect
               (and
                (not (on ?cupcake ?loc))
                (holding ?arm ?cupcake)
                (not (arm-empty))
                ))))
    (is (equalp '(and
                  (on ?arm ?loc)
                  (on ?cupcake ?loc)
                  (arm-empty)
                  )
                (primitive-precond act)))))

(test extract-op-precond
  (let ((op '(:op (!move ?agent ?from-x ?from-y ?to-x ?to-y)
              ;; Unknown effect: when the agent hits a wall it
              ;; remains at FROM-X, FROM-Y.
              ;; Unknown effect: when the agent can move,
              ;; it may end up in a diagonal location.
              ;; Unkown effect: when the agent gets over an active
              ;; RED mine, it's dead.
              :precond (and (explorer ?agent)
                        (not (eval (format t "~%Agent ~s at (~s ~s) to (~s ~s)..."
                                    '?agent '?from-x '?from-y '?to-x '?to-y)))
                        (at ?agent ?from-x ?from-y))
              :delete ((at ?agent ?from-x ?from-y))
              :add ((at ?agent ?to-x ?to-y)))))
    (is (equalp '(and (explorer ?agent)
                  (not (eval (format t "~%Agent ~s at (~s ~s) to (~s ~s)..."
                              '?agent '?from-x '?from-y '?to-x '?to-y)))
                  (at ?agent ?from-x ?from-y))
                (primitive-precond op)))))

(test extract-operator-precond
  (let ((operator '(:operator (!placeblock ?location ?t)
                              ;; preconditions
                              (
                               (type_location ?location) (type_blocktype ?t)
                               (empty ?location)
                               )
                              ;; delete effects
                              ((empty ?location))
                              ;; add effects
                              ((blockat ?location ?t))
                              1
                              )))
    (is (equalp '(and
                  (type_location ?location)
                  (type_blocktype ?t)
                  (empty ?location))
                (primitive-precond operator)))))

(test set-action-precond
  (let ((act '(:action pick-up
               :parameters
               (?arm - bot
                ?cupcake - locatable
                ?loc - location)
               :precondition
               (and
                (on ?arm ?loc)
                (on ?cupcake ?loc)
                (arm-empty)
                )
               :effect
               (and
                (not (on ?cupcake ?loc))
                (holding ?arm ?cupcake)
                (not (arm-empty))
                ))))
    (setf (primitive-precond act) '(and (foo bar)))
    (is (equalp '(and (foo bar))
                (primitive-precond act)))))

(test set-op-precond
  (let ((op '(:op (!move ?agent ?from-x ?from-y ?to-x ?to-y)
              ;; Unknown effect: when the agent hits a wall it
              ;; remains at FROM-X, FROM-Y.
              ;; Unknown effect: when the agent can move,
              ;; it may end up in a diagonal location.
              ;; Unkown effect: when the agent gets over an active
              ;; RED mine, it's dead.
              :precond (and (explorer ?agent)
                        (not (eval (format t "~%Agent ~s at (~s ~s) to (~s ~s)..."
                                    '?agent '?from-x '?from-y '?to-x '?to-y)))
                        (at ?agent ?from-x ?from-y))
              :delete ((at ?agent ?from-x ?from-y))
              :add ((at ?agent ?to-x ?to-y)))))
    (setf (primitive-precond op) '(and (foo bar)))
    (is (equalp '(and (foo bar))
                (primitive-precond op)))))

(test set-operator-precond
  (let ((operator '(:operator (!placeblock ?location ?t)
                              ;; preconditions
                              (
                               (type_location ?location) (type_blocktype ?t)
                               (empty ?location)
                               )
                              ;; delete effects
                              ((empty ?location))
                              ;; add effects
                              ((blockat ?location ?t))
                              1 ; cost
                              )))
    (setf (primitive-precond operator) '(and (foo bar)))
    (is (equalp '(and (foo bar))
                (primitive-precond operator)))
    (setf (primitive-precond operator) '(foo bar))
    (is (equalp '(foo bar)
                (primitive-precond operator)))
    (setf (primitive-precond operator) '((foo bar) (baz bletch)))
    (is (equalp '(and (foo bar) (baz bletch))
                (primitive-precond operator)))))

(test extract-action-name
  (let ((act '(:action pick-up
               :parameters
               (?arm - bot
                ?cupcake - locatable
                ?loc - location)
               :precondition
               (and
                (on ?arm ?loc)
                (on ?cupcake ?loc)
                (arm-empty)
                )
               :effect
               (and
                (not (on ?cupcake ?loc))
                (holding ?arm ?cupcake)
                (not (arm-empty))
                ))))
    (is (eq 'pick-up
            (primitive-name act)))))

(test extract-op-name
  (let ((op '(:op (!move ?agent ?from-x ?from-y ?to-x ?to-y)
              ;; Unknown effect: when the agent hits a wall it
              ;; remains at FROM-X, FROM-Y.
              ;; Unknown effect: when the agent can move,
              ;; it may end up in a diagonal location.
              ;; Unkown effect: when the agent gets over an active
              ;; RED mine, it's dead.
              :precond (and (explorer ?agent)
                        (not (eval (format t "~%Agent ~s at (~s ~s) to (~s ~s)..."
                                    '?agent '?from-x '?from-y '?to-x '?to-y)))
                        (at ?agent ?from-x ?from-y))
              :delete ((at ?agent ?from-x ?from-y))
              :add ((at ?agent ?to-x ?to-y)))))
    (is (eq '!move
            (primitive-name op)))))

(test extract-operator-name
  (let ((operator '(:operator (!placeblock ?location ?t)
                              ;; preconditions
                              (
                               (type_location ?location) (type_blocktype ?t)
                               (empty ?location)
                               )
                              ;; delete effects
                              ((empty ?location))
                              ;; add effects
                              ((blockat ?location ?t))
                              1
                              )))
    (is (eq '!placeblock
                (primitive-name operator)))))

(test set-action-name
  (let ((act '(:action pick-up
               :parameters
               (?arm - bot
                ?cupcake - locatable
                ?loc - location)
               :precondition
               (and
                (on ?arm ?loc)
                (on ?cupcake ?loc)
                (arm-empty)
                )
               :effect
               (and
                (not (on ?cupcake ?loc))
                (holding ?arm ?cupcake)
                (not (arm-empty))
                ))))
    (setf (primitive-name act) 'new-pick-up)
    (is (eq 'new-pick-up
            (primitive-name act)))))

(test set-op-name
  (let ((op '(:op (!move ?agent ?from-x ?from-y ?to-x ?to-y)
              ;; Unknown effect: when the agent hits a wall it
              ;; remains at FROM-X, FROM-Y.
              ;; Unknown effect: when the agent can move,
              ;; it may end up in a diagonal location.
              ;; Unkown effect: when the agent gets over an active
              ;; RED mine, it's dead.
              :precond (and (explorer ?agent)
                        (not (eval (format t "~%Agent ~s at (~s ~s) to (~s ~s)..."
                                    '?agent '?from-x '?from-y '?to-x '?to-y)))
                        (at ?agent ?from-x ?from-y))
              :delete ((at ?agent ?from-x ?from-y))
              :add ((at ?agent ?to-x ?to-y)))))
    (setf (primitive-name op) '!new-move)
    (is (eq '!new-move
            (primitive-name op)))))

(test extract-operator-name
  (let ((operator '(:operator (!placeblock ?location ?t)
                              ;; preconditions
                              (
                               (type_location ?location) (type_blocktype ?t)
                               (empty ?location)
                               )
                              ;; delete effects
                              ((empty ?location))
                              ;; add effects
                              ((blockat ?location ?t))
                              1
                              )))
    (setf (primitive-name operator) '!new-placeblock)
    (is (eq '!new-placeblock
            (primitive-name operator)))))

(test new-prim
  (let ((executed '(!drive A C))
        (new-prim-name '!drive-0-prime)
        (old-prim '(:operator (!drive ?l1 ?l2)
                    ;; preconditions
                    (
                     (type_location ?l1) (type_location ?l2)
                     (at ?l1) (not (toll-area ?l1)) (road ?l1 ?l2)
                     )
                    ;; delete effects
                    ((at ?l1))
                    ;; add effects
                    ((at ?l2))
                    1
                    ))
        (ordering-literal 'l1)
        (next-ordering-literal 'l2))
    (is
     (equalp
      `(:operator (,new-prim-name a c)
        ;; preconditions
        (and
         (l1)
         (and (type_location a) (type_location c)
          (at a) (not (toll-area a)) (road a c)))
        ;; delete effects
        ((l1) (at a))
        ;; add effects
        ((l2) (at c))
        1)
      (shop-plan-repair-rewrite::make-new-prim executed new-prim-name old-prim ordering-literal next-ordering-literal)))))

(test test-grouping
  (let ((old-action-names '(!NAVIGATE !NAVIGATE
                            !SAMPLE_SOIL !COMMUNICATE_SOIL_DATA
                            !NAVIGATE !NAVIGATE))
        (new-actions '((:ACTION !NAVIGATE-0-PRIME :PARAMETERS
                        (?X - ROVER ?Y -
                         WAYPOINT ?Z - WAYPOINT)
                        :PRECONDITION
                        (AND (SHOP3-USER::L-0) (= ?X ROVER0)
                         (= ?Y WAYPOINT3)
                         (= ?Z WAYPOINT1)
                         (AND (CAN_TRAVERSE ?X
                               ?Y ?Z)
                          (AVAILABLE ?X)
                          (AT ?X ?Y)
                          (VISIBLE ?Y ?Z)))
                        :EFFECT
                        (AND (AND (AND (NOT (AT ?X
                                                ?Y))
                                   (AT ?X ?Z))
                              (NOT (SHOP3-USER::L-0)))
                         (SHOP3-USER::L-1)))
                       (:ACTION !NAVIGATE-1-PRIME :PARAMETERS
                        (?X - ROVER ?Y -
                         WAYPOINT ?Z - WAYPOINT)
                        :PRECONDITION
                        (AND (SHOP3-USER::L-1) (= ?X ROVER0)
                         (= ?Y WAYPOINT1)
                         (= ?Z WAYPOINT2)
                         (AND (CAN_TRAVERSE ?X
                               ?Y ?Z)
                          (AVAILABLE ?X)
                          (AT ?X ?Y)
                          (VISIBLE ?Y ?Z)))
                        :EFFECT
                        (AND (AND (AND (NOT (AT ?X
                                                ?Y))
                                   (AT ?X ?Z))
                              (NOT (SHOP3-USER::L-1)))
                         (SHOP3-USER::L-2)))
                       (:ACTION !SAMPLE_SOIL-2-PRIME :PARAMETERS
                        (?X - ROVER ?S -
                         STORE ?P - WAYPOINT)
                        :PRECONDITION
                        (AND (SHOP3-USER::L-2) (= ?X ROVER0)
                         (= ?S ROVER0STORE)
                         (= ?P WAYPOINT2)
                         (AND (AT ?X ?P)
                          (AT_SOIL_SAMPLE ?P)
                          (EQUIPPED_FOR_SOIL_ANALYSIS ?X)
                          (STORE_OF ?S ?X)
                          (EMPTY ?S)))
                        :EFFECT
                        (AND (AND (AND (NOT (EMPTY ?S))
                                   (FULL ?S)
                                   (HAVE_SOIL_ANALYSIS ?X
                                    ?P)
                                   (NOT (AT_SOIL_SAMPLE ?P)))
                              (NOT (SHOP3-USER::L-2)))
                         (SHOP3-USER::L-3)))
                       (:ACTION !COMMUNICATE_SOIL_DATA-3-PRIME :PARAMETERS
                        (?R - ROVER ?L -
                         LANDER ?P - WAYPOINT
                         ?X - WAYPOINT ?Y -
                         WAYPOINT)
                        :PRECONDITION
                        (AND (SHOP3-USER::L-3) (= ?R ROVER0)
                         (= ?L GENERAL)
                         (= ?P WAYPOINT2)
                         (= ?X WAYPOINT2)
                         (= ?Y WAYPOINT0)
                         (AND (AT ?R ?X)
                          (AT_LANDER ?L ?Y)
                          (HAVE_SOIL_ANALYSIS ?R
                           ?P)
                          (VISIBLE ?X ?Y)
                          (AVAILABLE ?R)
                          (CHANNEL_FREE ?L)))
                        :EFFECT
                        (AND (AND (AND (CHANNEL_FREE ?L)
                                   (COMMUNICATED_SOIL_DATA
                                    ?P)
                                   (AVAILABLE ?R))
                              (NOT (SHOP3-USER::L-3)))
                         (SHOP3-USER::L-4)))
                       (:ACTION !NAVIGATE-4-PRIME :PARAMETERS
                        (?X - ROVER ?Y -
                         WAYPOINT ?Z - WAYPOINT)
                        :PRECONDITION
                        (AND (SHOP3-USER::L-4) (= ?X ROVER0)
                         (= ?Y WAYPOINT2)
                         (= ?Z WAYPOINT1)
                         (AND (CAN_TRAVERSE ?X
                               ?Y ?Z)
                          (AVAILABLE ?X)
                          (AT ?X ?Y)
                          (VISIBLE ?Y ?Z)))
                        :EFFECT
                        (AND (AND (AND (NOT (AT ?X
                                                ?Y))
                                   (AT ?X ?Z))
                              (NOT (SHOP3-USER::L-4)))
                         (SHOP3-USER::L-5)))
                       (:ACTION !NAVIGATE-5-PRIME :PARAMETERS
                        (?X - ROVER ?Y -
                         WAYPOINT ?Z - WAYPOINT)
                        :PRECONDITION
                        (AND (SHOP3-USER::L-5) (= ?X ROVER0)
                         (= ?Y WAYPOINT1)
                         (= ?Z WAYPOINT3)
                         (AND (CAN_TRAVERSE ?X
                               ?Y ?Z)
                          (AVAILABLE ?X)
                          (AT ?X ?Y)
                          (VISIBLE ?Y ?Z)))
                        :EFFECT
                        (AND (AND (AND (AND (NOT (AT ?X
                                                     ?Y))
                                        (AT ?X
                                            ?Z))
                                   (NOT (SHOP3-USER::L-5)))
                              (SHOP3-USER::L-6))
                         (NOT (VISIBLE WAYPOINT3
                               WAYPOINT0)))))))
    (is (alexandria:set-equal
         (pairlis '(!navigate !SAMPLE_SOIL !COMMUNICATE_SOIL_DATA)
                  '(((:ACTION !NAVIGATE-5-PRIME :PARAMETERS
                      (?X - ROVER ?Y -
                       WAYPOINT ?Z - WAYPOINT)
                      :PRECONDITION
                      (AND (SHOP3-USER::L-5) (= ?X ROVER0)
                       (= ?Y WAYPOINT1)
                       (= ?Z WAYPOINT3)
                       (AND (CAN_TRAVERSE ?X
                             ?Y ?Z)
                        (AVAILABLE ?X)
                        (AT ?X ?Y)
                        (VISIBLE ?Y ?Z)))
                      :EFFECT
                      (AND (AND (AND (AND (NOT (AT ?X
                                                   ?Y))
                                          (AT ?X
                                              ?Z))
                                 (NOT (SHOP3-USER::L-5)))
                            (SHOP3-USER::L-6))
                       (NOT (VISIBLE WAYPOINT3
                             WAYPOINT0))))
                     (:ACTION !NAVIGATE-4-PRIME :PARAMETERS
                      (?X - ROVER ?Y -
                       WAYPOINT ?Z - WAYPOINT)
                      :PRECONDITION
                      (AND (SHOP3-USER::L-4) (= ?X ROVER0)
                       (= ?Y WAYPOINT2)
                       (= ?Z WAYPOINT1)
                       (AND (CAN_TRAVERSE ?X
                             ?Y ?Z)
                        (AVAILABLE ?X)
                        (AT ?X ?Y)
                        (VISIBLE ?Y ?Z)))
                      :EFFECT
                      (AND (AND (AND (NOT (AT ?X
                                              ?Y))
                                 (AT ?X ?Z))
                            (NOT (SHOP3-USER::L-4)))
                       (SHOP3-USER::L-5)))
                     (:ACTION !NAVIGATE-1-PRIME :PARAMETERS
                      (?X - ROVER ?Y -
                       WAYPOINT ?Z - WAYPOINT)
                      :PRECONDITION
                      (AND (SHOP3-USER::L-1) (= ?X ROVER0)
                       (= ?Y WAYPOINT1)
                       (= ?Z WAYPOINT2)
                       (AND (CAN_TRAVERSE ?X
                             ?Y ?Z)
                        (AVAILABLE ?X)
                        (AT ?X ?Y)
                        (VISIBLE ?Y ?Z)))
                      :EFFECT
                      (AND (AND (AND (NOT (AT ?X
                                              ?Y))
                                 (AT ?X ?Z))
                            (NOT (SHOP3-USER::L-1)))
                       (SHOP3-USER::L-2)))
                     (:ACTION !NAVIGATE-0-PRIME :PARAMETERS
                      (?X - ROVER ?Y -
                       WAYPOINT ?Z - WAYPOINT)
                      :PRECONDITION
                      (AND (SHOP3-USER::L-0) (= ?X ROVER0)
                       (= ?Y WAYPOINT3)
                       (= ?Z WAYPOINT1)
                       (AND (CAN_TRAVERSE ?X
                             ?Y ?Z)
                        (AVAILABLE ?X)
                        (AT ?X ?Y)
                        (VISIBLE ?Y ?Z)))
                      :EFFECT
                      (AND (AND (AND (NOT (AT ?X
                                              ?Y))
                                 (AT ?X ?Z))
                            (NOT (SHOP3-USER::L-0)))
                       (SHOP3-USER::L-1))))
                    ((:ACTION !SAMPLE_SOIL-2-PRIME :PARAMETERS
                      (?X - ROVER ?S -
                       STORE ?P - WAYPOINT)
                      :PRECONDITION
                      (AND (SHOP3-USER::L-2) (= ?X ROVER0)
                       (= ?S ROVER0STORE)
                       (= ?P WAYPOINT2)
                       (AND (AT ?X ?P)
                        (AT_SOIL_SAMPLE ?P)
                        (EQUIPPED_FOR_SOIL_ANALYSIS ?X)
                        (STORE_OF ?S ?X)
                        (EMPTY ?S)))
                      :EFFECT
                      (AND (AND (AND (NOT (EMPTY ?S))
                                 (FULL ?S)
                                 (HAVE_SOIL_ANALYSIS ?X
                                                     ?P)
                                 (NOT (AT_SOIL_SAMPLE ?P)))
                            (NOT (SHOP3-USER::L-2)))
                       (SHOP3-USER::L-3))))
                    ((:ACTION !COMMUNICATE_SOIL_DATA-3-PRIME :PARAMETERS
                        (?R - ROVER ?L -
                         LANDER ?P - WAYPOINT
                         ?X - WAYPOINT ?Y -
                         WAYPOINT)
                        :PRECONDITION
                        (AND (SHOP3-USER::L-3) (= ?R ROVER0)
                         (= ?L GENERAL)
                         (= ?P WAYPOINT2)
                         (= ?X WAYPOINT2)
                         (= ?Y WAYPOINT0)
                         (AND (AT ?R ?X)
                          (AT_LANDER ?L ?Y)
                          (HAVE_SOIL_ANALYSIS ?R
                           ?P)
                          (VISIBLE ?X ?Y)
                          (AVAILABLE ?R)
                          (CHANNEL_FREE ?L)))
                        :EFFECT
                        (AND (AND (AND (CHANNEL_FREE ?L)
                                   (COMMUNICATED_SOIL_DATA
                                    ?P)
                                   (AVAILABLE ?R))
                              (NOT (SHOP3-USER::L-3)))
                         (SHOP3-USER::L-4))))))
         (shop-plan-repair-rewrite::regroup-action-lists new-actions old-action-names)
         :test 'equalp))))
