
 ; evaluar contiene:  evaluar, generar-resolventes-bis, pertenece-vacia,
 ;                    pertenece-vacia-bis

(de evaluar (expandidas expandibles &aux resolventes)
   (monitor expandidas expandibles)
   (interrupcion)
   (if expandibles
       (if (pertenece-vacia-bis
               (setq resolventes
                     (if (not (tope-profundidad (first expandibles)))
                         (generar-resolventes-bis expandidas expandibles)
                     )
               )
           )
           'true
           (evaluar (obtener-expandidas expandidas
                                        (first expandibles)
                                        heuristica-secundaria
                    )
                    (obtener-expandibles expandibles
                                         (admisibles resolventes
                                                     heuristica-primaria
                                         )
                                         heuristica-primaria
                    )
           )
       )
   )
)

(de generar-resolventes-bis (expandidas expandibles)
   (auto-depurar-subsumidas
      (eliminar-subsumidas
         (fusionar-clausulas
            (eliminar-tautologias
               (expandir (first expandibles) expandidas)
            )
         )
         (append expandidas (rest expandibles))
      )
   )
)

(de pertenece-vacia (clausulas &aux (clausula (first clausulas)))
   (if clausulas
       (or (and (null (first clausula)) (null (second clausula)))
           (pertenece-vacia (rest clausulas))
       )
   )
)

(de pertenece-vacia-bis (clausulas &aux (clausula (first clausulas)))
   (and clausulas (null (first clausula)) (null (second clausula))))
)

(print 'evaluar-ok)