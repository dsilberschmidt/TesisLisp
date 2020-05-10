
 ; renombrar contiene:  renombrar, gensym, extraer-raiz, hallar-variables,
 ;                      hallar-raices, normalizar, inicializar-gensym,
 ;                      renombrar-anonimas, sacar-anonimas

 ; (load "unificar")   necesita: variablep, aplicar

 ; (load "uti")        necesita: union, map-2-ar

 ; (load "uticla")     necesita: renombrar-anonima

(de renombrar (expresion &aux (variables (hallar-variables expresion)))
   (aplicar (map-2-ar list variables (mapcar gensym variables)) expresion)
)

(de gensym (simbolo &aux (raiz (extraer-raiz simbolo)) posicion)
   (if (setq posicion (memvec raiz vector-raices))
       (insert (string raiz \_ ( (store vector-numeros
                                        posicion
                                        (add1 (vector-numeros posicion))
                                 )
                                 posicion
                               )
               )
        )
        (setq vector-raices (vector vector-raices simbolo))
        (setq vector-numeros (vector vector-numeros 1))
        (insert (string simbolo \_ 1))
    )
)

(de extraer-raiz (simbolo &aux (hilera (pname simbolo))
                               (posicion (string-search hilera \_))
                 )
   (insert (if posicion
               (substring hilera 1 (sub1 posicion))
               hilera
           )
   )
)

(de hallar-variables (expresion)
   (if (variablep expresion)
       (list expresion)
       (if (listp expresion)
           (union (hallar-variables (first expresion))
                  (hallar-variables (rest expresion))
           )
       )
   )
)

(de hallar-raices (expresion)
   (mapcar extraer-raiz (hallar-variables expresion))
)

(de normalizar (clausulas)
   (mapcar renombrar (renombrar-anonima clausulas))
)

(de inicializar-gensym ()
   (setq vector-raices [])
   (setq vector-numeros [])
)

(de renombrar-anonima (expr)
   (cond
      ((= expr '?) (gensym '?))
      ((atom expr) expr)
      (t (cons (renombrar-anonima (first expr))
               (renombrar-anonima (rest expr))
         )
      )
   )
)

(de sacar-anonimas (lista)
   (cond
      ( (null lista) nil )
      ( (= (extraer-raiz (first lista)) '?) (sacar-anonimas (rest lista)) )
      ( t  (cons (first lista) (sacar-anonimas (rest lista))) )
   )
)

(print 'renombrar-ok)

