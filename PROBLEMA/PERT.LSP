
(setq pert '(  (paratodo (x) (x pert (cons x ?)))
               (paratodo (x w z)
                     ((x pert z) -> (x pert (cons w z)))
               )
            )
)
