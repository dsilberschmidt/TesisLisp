
(setq dama3
     '(  (  ((hay habitacion1 tigre) or (hay habitacion2 dama))
            <->
            (hay habitacion1 dama)
         )
         (paratodo (x) ((hay x dama) xor (hay x tigre)))
      )
)
