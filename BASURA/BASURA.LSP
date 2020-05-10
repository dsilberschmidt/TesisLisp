
(de pertenece-vacia (clausulas &aux (primer-clausula (first clausulas)))
   (if clausulas
       (if (and (null (first primer-clausula))
                (null (second primer-clausula))
           )
           'true
           (pertenece-vacia (rest clausulas))
       )
   )
)



(de pertenece-vacia* (clausulas)
   (not (map-2-and or (mapcar first clausulas)
                      (mapcar second clausulas)
        )
   )
)

