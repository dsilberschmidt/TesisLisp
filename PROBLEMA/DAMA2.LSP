
(setq dama2
     '(  (  ((hay habitacion1 dama) or (hay habitacion2 dama))
            <->
            (hay habitacion1 tigre)
         )
         (paratodo (x) ((hay x dama) xor (hay x tigre)))
      )
)
