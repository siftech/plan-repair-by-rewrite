(in-package :shop-user)
(defdomain domain (
  (:operator (!drive ?l1 ?l2)
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
  )
  (:operator (!drive-ta ?l1 ?l2)
    ;; preconditions
    (
      (type_location ?l1) (type_location ?l2)
      (at ?l1) (toll-area ?l1) (road ?l1 ?l2)
    )
    ;; delete effects
    ((at ?l1))
    ;; add effects
    ((at ?l2))
    1
  )
  (:operator (!pay-toll ?l)
    ;; preconditions
    (
      (type_location ?l)
      (at ?l) (pay-location ?l)
    )
    ;; delete effects
    ()
    ;; add effects
    ()
    1
  )
  (:method (__top)
    __top_method
    (

      (type_sort_for_a ?var_for_a_1) (type_sort_for_h ?var_for_h_2)

    )
    ((move ?var_for_a_1 ?var_for_h_2))
  )
  (:method (move ?l1 ?l2)
    move-one
    (
      (type_location ?l1) (type_location ?l2)
      (type_location ?l1) (type_location ?l2)

    )
    ((!drive ?l1 ?l2))
  )
  (:method (move ?l1 ?l3)
    move-more
    (
      (type_location ?l1) (type_location ?l3)
      (type_location ?l1) (type_location ?l2) (type_location ?l3)

    )
    ((!drive ?l1 ?l2) (move ?l2 ?l3))
  )
  (:method (move ?l1 ?l1)
    move-none
    (
      (type_location ?l1) (type_location ?l1)
      (type_location ?l1)

    )
    ()
  )
  (:method (move ?l1 ?l2)
    move-one-ta
    (
      (type_location ?l1) (type_location ?l2)
      (type_location ?l1) (type_location ?l2)

    )
    ((!drive-ta ?l1 ?l2) (!pay-toll ?l2))
  )
  (:method (move ?l1 ?l3)
    move-more-ta
    (
      (type_location ?l1) (type_location ?l3)
      (type_location ?l1) (type_location ?l2) (type_location ?l3)

    )
    ((!drive-ta ?l1 ?l2) (move ?l2 ?l3) (!pay-toll ?l3))
  )
))
