
 ; estrategia contiene: ancho, soporte, sin-restriccion, entrada-lineal,
 ;                      restriccion-fusion, tope-profundidad, ancestro,
 ;                      constante, menores, unidades, horizontal, vertical

 ; tipos de llamada

(de ancho (base goal)
   (busqueda nil (append base goal) (sacar-anonimas (hallar-raices goal)))
)

(de soporte (base goal)
   (busqueda base goal (sacar-anonimas (hallar-raices goal)))
)

 ; tipos de seleccion

(de sin-restriccion (clausula) t)

(de entrada-lineal (clausula)
   (= (sixth clausula) 0)
)

(de restriccion-fusion (clausula)
   (or (eq (seventh clausula) 'fusion)
       (= (sixth clausula) 0)
   )
)

 ; control de profundidad

(de tope-profundidad (clausula)
   (if limite-profundidad
       (= limite-profundidad (sixth clausula))
   )
)

 ; filtrado de ancestros

(de ancestro (clausula1 clausula2)
   (and clausula2
        (or clausula1 clausula2)
        (ancestro clausula1 (fourth clausula2))
        (ancestro clausula1 (fifth clausula2))
   )
)

 ; tipos de funcion-h

(de constante (clausula) 0)

(de menores (clausula)
   (+ (length (first clausula)) (length (second clausula)))
)

(de unidades (clausula)
   (if (= (+ (length (first clausula)) (length (second clausula))) 1)
       0
       1
   )
)
 ; tipos de prioridad

(de horizontal (a b)
   (cl:< a b)
)

(de vertical (a b)
   (cl:<= a b)
)

(print 'estrategia-ok)
