
(setq dama1
      '(  (  ((hay habitacion1 dama) and (hay habitacion2 tigre))
             xor
             (    ((hay habitacion1 dama) and (hay habitacion2 tigre))
                  xor
                  ((hay habitacion2 dama) and (hay habitacion1 tigre))
             )
          )
          (paratodo (x) ((hay x dama) xor (hay x tigre)))
       )
)
