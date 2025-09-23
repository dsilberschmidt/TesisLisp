
 ; tautologia contiene: tautologiap, eliminar-tautologias

 ; (load "uti")         necesita: interseccion


(de tautologiap (clausula)
   (and flag-control-tautologia
        (interseccion (first clausula) (second clausula))
   )
)

(de eliminar-tautologias (clausulas)
   (if clausulas
      (if (tautologiap (first clausulas))
          (eliminar-tautologias (rest clausulas))
          (cons (first clausulas) (eliminar-tautologias (rest clausulas)))
      )
   )
)

(print 'tautologia-ok)
