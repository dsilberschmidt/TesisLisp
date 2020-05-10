
(setq dama4
     '(  ( (l1) <-> ((hay habitacion1 dama) and (hay habitacion2 dama)) )
         ( (l2) <-> ((hay habitacion1 dama) and (hay habitacion2 dama)) )
         ( (hay habitacion1 dama)  <-> (l1) )
         ( (hay habitacion2 tigre) <-> (l2) )
         ( paratodo (x) ((hay x dama) xor (hay x tigre)))
      )
)
