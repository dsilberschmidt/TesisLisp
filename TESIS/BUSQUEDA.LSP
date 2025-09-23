
 ; busqueda contiene: busqueda, obtener-expandidas, obtener-expandibles,
 ;                    colocar, ordenar, expandir, generar-resolventes,
 ;                    admisibles

(de  busqueda (expandidas expandibles raices &aux resolventes (flag-mas t))
   (monitor expandidas expandibles)
   (interrupcion)
   (if (null expandibles)
       (print2 "No hay (mas) respuestas.")
       (setq resolventes
             (if (not (tope-profundidad (first expandibles)))
                 (generar-resolventes expandidas expandibles raices)
             )
       )
       (if flag-mas
           (busqueda (obtener-expandidas expandidas
                                         (first expandibles)
                                         heuristica-secundaria
                     )
                     (obtener-expandibles expandibles
                                          (admisibles resolventes
                                                      heuristica-primaria
                                          )
                                          heuristica-primaria
                     )
                     raices
           )
       )
   )
)

(de obtener-expandidas (expandidas-viejas aspirante heuristica
                        &aux (admisibilidad (first heuristica))
                       )
   (if (and (funcall admisibilidad aspirante)
            (not (subsumida aspirante expandidas-viejas))
       )
       (colocar aspirante
                (eliminar-subsumidas expandidas-viejas (list aspirante))
                heuristica
       )
       expandidas-viejas
   )
)

(de obtener-expandibles (expandibles-viejas resolventes heuristica)
   (ordenar resolventes
            (eliminar-subsumidas (rest expandibles-viejas) resolventes)
            heuristica
   )
)

(de colocar (elemento lista heuristica &aux (funcion-h (second heuristica))
                                            (prioridad (third heuristica))
            )
   (if (or (null lista)
           (funcall prioridad (funcall funcion-h elemento)
                    (funcall funcion-h (first lista)))
       )
       (cons elemento lista)
       (cons (first lista)
             (colocar elemento (rest lista) heuristica)
       )
   )
)

(de ordenar (lista lista-ordenada heuristica)
   (if lista
       (ordenar (rest lista)
                (colocar (first lista) lista-ordenada heuristica)
                heuristica
       )
       lista-ordenada
   )
)

(de expandir (clausula clausulas)
   (if clausulas
       (if (or (not flag-filtrado-ancestros)
               (= (sixth (first clausulas)) 0)
               (ancestro clausula (first clausulas))
           )
           (append (resolver-completo clausula (first clausulas))
                   (expandir clausula (rest clausulas))
           )
           (expandir clausula (rest clausulas))
       )
   )
)

(de generar-resolventes (expandidas expandibles raices)
   (auto-depurar-subsumidas
      (eliminar-subsumidas
         (eliminar-respuestas-y-mostrarlas               ; modifica flag-mas
            (fusionar-clausulas
               (eliminar-tautologias
                   (expandir (first expandibles) expandidas)
               )
            )
            raices
         )
         (append expandidas (rest expandibles))
      )
   )
)

(de admisibles (lista heuristica &aux (admisibilidad (first heuristica)))
   (if lista
       (if (funcall admisibilidad (first lista))
           (cons (first lista) (admisibles (rest lista) heuristica))
           (admisibles (rest lista) heuristica)
       )
   )
)

(print 'busqueda-ok)
