
 ; renombrar contiene:  renombrar, gensym, extraer-raiz, hallar-variables,
 ;                      hallar-raices, normalizar, inicializar-gensym,
 ;                      renombrar-anonimas, sacar-anonimas

 ; (load "unificar")   necesita: variablep, aplicar

 ; (load "uti")        necesita: union, map-2-ar

 ; (load "uticla")     necesita: renombrar-anonima

(defvar gensym-registry (make-hash-table :test #'eq))

(de inicializar-gensym ()
   (clrhash gensym-registry)
)

(de extraer-raiz (simbolo)
   (let* ((nombre (pname simbolo))
          (posicion (string-search nombre #\_)))
     (insert (if posicion
                 (substring nombre 1 (sub1 posicion))
                 nombre)))
)

(de proximo-nombre (simbolo)
   (let* ((raiz (extraer-raiz simbolo))
          (contador (gethash raiz gensym-registry 0))
          (nuevo (add1 contador)))
     (setf (gethash raiz gensym-registry) nuevo)
     (insert (string raiz #\_ nuevo)))
)

(de gensym (simbolo)
   (proximo-nombre simbolo)
)

(de renombrar (expresion &aux (variables (hallar-variables expresion)))
   (aplicar (map-2-ar #'list variables (mapcar #'gensym variables)) expresion)
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
   (mapcar #'extraer-raiz (hallar-variables expresion))
)

(de normalizar (clausulas)
   (mapcar #'renombrar (renombrar-anonima clausulas))
)

(de renombrar-anonima (expr)
   (cond
      ((eq expr '?) (gensym '?))
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
      ( (eq (extraer-raiz (first lista)) '?) (sacar-anonimas (rest lista)) )
      ( t  (cons (first lista) (sacar-anonimas (rest lista))) )
   )
)

(print 'renombrar-ok)
