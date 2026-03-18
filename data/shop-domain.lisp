(defdomain domain (
  (:operator (!drive ?l1 ?l2)
    ;; preconditions
    (
      (type_location ?l1) (type_location ?l2)
      (at ?l1) (not (toll-area ?l1)) (road ?l1 ?l2) (l-2)
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
      (at ?l1) (toll-area ?l1) (road ?l1 ?l2) (l-2)
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
      (at ?l) (pay-location ?l) (l-2)
    )
    ;; delete effects
    ()
    ;; add effects
    ()
    1
  )
  (:operator (!drive-1-prime ?l1 ?l2)
    ;; preconditions
    (
      (type_location ?l1) (type_location ?l2)
      (at ?l1) (not (toll-area ?l1)) (road ?l1 ?l2) (l-0)
    )
    ;; delete effects
    ((at ?l1) (l-0))
    ;; add effects
    ((at ?l2) (l-1))
    1
  )
  (:operator (!drive-ta-3-prime ?l1 ?l2 ?var_for_f ?var_for_g)
    ;; preconditions
    (
      (type_location ?l1) (type_location ?l2) (type_sort_for_f ?var_for_f) (type_sort_for_g ?var_for_g)
      (at ?l1) (toll-area ?l1) (road ?l1 ?l2) (l-1)
    )
    ;; delete effects
    ((at ?l1) (l-1) (road ?var_for_g ?var_for_f))
    ;; add effects
    ((at ?l2) (l-2))
    1
  )
  (:method (__top)
    __top_method
    (
      
      (type_sort_for_a ?var_for_a_1) (type_sort_for_c ?var_for_c_2) (type_sort_for_c ?var_for_c_3) (type_sort_for_g ?var_for_g_4) (type_sort_for_f ?var_for_f_method___top_method_instance_5) (type_sort_for_g ?var_for_g_method___top_method_instance_6) (type_sort_for_e ?var_for_e_7) (type_sort_for_g ?var_for_g_8)
      
    )
    ((!drive-1-prime ?var_for_a_1 ?var_for_c_2) (!drive-ta-3-prime ?var_for_c_3 ?var_for_g_4 ?var_for_f_method___top_method_instance_5 ?var_for_g_method___top_method_instance_6) (!drive-ta ?var_for_g_8 ?var_for_e_7))
  )
  (:method (move ?l1 ?l2)
    move-one
    (
      (type_location ?l1) (type_location ?l2)
      (type_location ?l1) (type_location ?l2)
      
    )
    ((replay-drive-1-prime ?l1 ?l2))
  )
  (:method (move ?l1 ?l3)
    move-more
    (
      (type_location ?l1) (type_location ?l3)
      (type_location ?l1) (type_location ?l2) (type_location ?l3)
      
    )
    ((replay-drive-1-prime ?l1 ?l2) (move ?l2 ?l3))
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
    ((replay-drive-ta-3-prime ?l1 ?l2) (!pay-toll ?l2))
  )
  (:method (move ?l1 ?l3)
    move-more-ta
    (
      (type_location ?l1) (type_location ?l3)
      (type_location ?l1) (type_location ?l2) (type_location ?l3)
      
    )
    ((replay-drive-ta-3-prime ?l1 ?l2) (move ?l2 ?l3) (!pay-toll ?l3))
  )
  (:method (replay-drive-1-prime ?l1 ?l2)
    drive-1-prime0
    (
      (type_location ?l1) (type_location ?l2)
      (type_location ?l1) (type_location ?l2)
      
    )
    ((!drive-1-prime ?l1 ?l2))
  )
  (:method (replay-drive-1-prime ?l1 ?l2)
    drive-1-prime1
    (
      (type_location ?l1) (type_location ?l2)
      (type_location ?l1) (type_location ?l2)
      
    )
    ((!drive ?l1 ?l2))
  )
  (:method (replay-drive-ta-3-prime ?l1 ?l2)
    drive-ta-3-prime2
    (
      (type_location ?l1) (type_location ?l2)
      (type_location ?l1) (type_location ?l2) (type_sort_for_f ?var_for_f_method_drive-ta-3-prime2_instance_17) (type_sort_for_g ?var_for_g_method_drive-ta-3-prime2_instance_18)
      
    )
    ((!drive-ta-3-prime ?l1 ?l2 ?var_for_f_method_drive-ta-3-prime2_instance_17 ?var_for_g_method_drive-ta-3-prime2_instance_18))
  )
  (:method (replay-drive-ta-3-prime ?l1 ?l2)
    drive-ta-3-prime3
    (
      (type_location ?l1) (type_location ?l2)
      (type_location ?l1) (type_location ?l2)
      
    )
    ((!drive-ta ?l1 ?l2))
  )
))
