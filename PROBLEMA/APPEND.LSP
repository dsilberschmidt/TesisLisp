
(setq app '(  (paratodo (x) (append nil x x))
              (paratodo (x w z t)
                   ( (append x w z) -> (append (cons t x) w (cons t z)))
              )
           )
)
