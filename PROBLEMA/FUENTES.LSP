
 ; fuentes

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

(setq dama2
     '(  (  ((hay habitacion1 dama) or (hay habitacion2 dama))
            <->
            (hay habitacion1 tigre)
         )
         (paratodo (x) ((hay x dama) xor (hay x tigre)))
      )
)

(setq dama3
     '(  (  ((hay habitacion1 tigre) or (hay habitacion2 dama))
            <->
            (hay habitacion1 dama)
         )
         (paratodo (x) ((hay x dama) xor (hay x tigre)))
      )
)

(setq dama4
     '(  ( (l1) <-> ((hay habitacion1 dama) and (hay habitacion2 dama)) )
         ( (l2) <-> ((hay habitacion1 dama) and (hay habitacion2 dama)) )
         ( (hay habitacion1 dama)  <-> (l1) )
         ( (hay habitacion2 tigre) <-> (l2) )
         ( paratodo (x) ((hay x dama) xor (hay x tigre)))
      )
)


(setq dama5
     '(  ( (l1) <-> ((hay habitacion1 dama) or (hay habitacion2 dama)) )
         ( (l2) <-> (hay habitacion1 dama) )
         ( (hay habitacion1 dama)  <-> (l1) )
         ( (hay habitacion2 tigre) <-> (l2) )
         ( paratodo (x) ((hay x dama) xor (hay x tigre)))
      )
)

(setq dama6
     '(  ( paratodo (x)
               ( (l1) <-> ((hay habitacion1 x) <-> (hay habitacion2 x)) )
         )
         ( (l2) <-> (hay habitacion1 dama) )
         ( (hay habitacion1 dama) <-> (l1) )
         ( (hay habitacion2 tigre) <-> (l2) )
         ( paratodo (x) ((hay x dama) xor (hay x tigre)))
      )
)

(setq dama7
      '(  ( paratodo (x)
                ( (l1) <-> ((hay habitacion1 x) xor (hay habitacion2 x)) )
          )
          ( (l2) <-> ( (hay habitacion1 dama) and (hay habitacion2 tigre) ) )
          ( (hay habitacion1 dama) <-> (l1) )
          ( (hay habitacion2 tigre) <-> (l2) )
          ( paratodo (x) ((hay x dama) xor (hay x tigre)))
       )
)

(setq presidente1
     '(  ((presidente alfonsin) or (presidente bustelo))
         ((presidente alfonsin) or (presidente cafiero))
         (paratodo (x y) (((presidente x) and (presidente y)) -> (x = y)))
         (paratodo (x) (x = x))
      )
)

(setq presidente2
     '(  ( (presidente alfonsin) or (presidente bustelo) )
         ( (presidente alfonsin) or (presidente cafiero) )
         ( (presidente alfonsin)
                -> (not ((presidente bustelo) or (presidente cafiero)))
         )
         ( (presidente bustelo)
                -> (not ((presidente alfonsin) or (presidente cafiero)))
         )
         ( (presidente cafiero)
                -> (not ((presidente alfonsin) or (presidente bustelo)))
         )
      )
)

(setq transil1
      '( ((dice pepe) <-> (pepe es vampiro cuerdo))
         (paratodo (x) ((x es humano cuerdo)  -> (dice x)))
         (paratodo (x) ((x es humano loco)    -> (not (dice x))))
         (paratodo (x) ((x es vampiro cuerdo) -> (not (dice x))))
         (paratodo (x) ((x es vampiro loco)   -> (dice x)))
         (paratodo (x) ( ((x es humano cuerdo) xor (x es vampiro loco))
                         xor
                         ((x es vampiro cuerdo) xor (x es humano loco))
                       )
         )
       )
)

(setq transil2
   '( ((dice pepe) <-> ((pepe es vampiro) and (pepe esta cuerdo)))
      (paratodo (x) (((x es humano)  and (x esta cuerdo)) -> (dice x)))
      (paratodo (x) (((x es humano)  and (x esta loco))   -> (not (dice x))))
      (paratodo (x) (((x es vampiro) and (x esta cuerdo)) -> (not (dice x))))
      (paratodo (x) (((x es vampiro) and (x esta loco))   -> (dice x)))
      (paratodo (x) ((x es humano)   xor (x es vampiro)))
      (paratodo (x) ((x esta cuerdo) xor (x esta loco)))
    )
)

(setq app '(  (paratodo (x) (append nil x x))
              (paratodo (x w z t)
                   ( (append x w z) -> (append (cons t x) w (cons t z)))
              )
           )
)

(setq pert '(  (paratodo (x) (x pert (cons x ?)))
               (paratodo (x w z)
                     ((x pert z) -> (x pert (cons w z)))
               )
            )
)

(setq rev '(  (paratodo (x) (append nil x x))
              (paratodo (x w z t)
                   ( (append x w z) -> (append (cons t x) w (cons t z)))
              )
              (reverse nil nil)
              (paratodo (h t l z)
                   ( ((reverse t z) and (append z (cons h nil) l))
                         -> (reverse (cons h t) l)
                   )
              )
           )
)

(print 'fuentes-ok)
