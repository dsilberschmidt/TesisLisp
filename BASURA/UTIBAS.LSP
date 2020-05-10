
(df @ (implicacion)
   (armar-clausula (separar implicacion))
)

(df $ (implicaciones)
   (mapcar (lambda (implicacion) (armar-clausula (separar implicacion)))
           implicaciones
   )
)

(df ?? (implicacion &aux (clausula (separar implicacion))
                         (variables (hallar-variables clausula))
       )
   (mapcar (lambda (clause) (reemplazar clause 3 (list variables)))
           (append (mapcar (lambda (x) (apply @ `(<- ,x)))
                           (first clausula)
                   )
                   (mapcar (lambda (x) (apply @ `(,x)))
                           (second clausula)
                   )
           )
   )
)

(df gn (implicaciones &aux (variables (hallar-variables implicaciones)))
   (mapcar (lambda (clause) (reemplazar clause 3 (list variables)))
           (apply $ implicaciones)
   )
)

(de map-2-and (funcion lista1 lista2)
   (or (not lista1) (not lista2)
       (and (funcion (first lista1) (first lista2))
            (map-2-and funcion (rest lista1) (rest lista2))
       )
   )
)
